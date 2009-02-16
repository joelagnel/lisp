#!/usr/bin/env ruby

#
# Goals:
#
# 1) Simple
#    -No TCP
#    -No ACL
#    -Plain old pipes
#    -Simple commands
#    -Full 8-bit communication
#
# 2) Concurrency and callbacks handle in server
#    -Wait for value
#    -Define callback
#    -Persistent bindings
#
# 3) Use languages to their potential
#    -SEXPR's used for communication
#

=begin

From Ruby to Elisp:


Data is: SEXPR

SEXPR is a LISP list in the form:
(status options elisp)


STATUS is a symbol and is one of:

~  Raw data returned.
=  Finished Ruby execution.
!  Error with Ruby evaluation.
*  Continue (Ruby calling back elisp)
!!  Internal error. Maybe a bad packet.


OPTIONS is an ALIST with the supported keys

t  Transaction used. Only return with a Continue response.
m  Marhsalling used. Only returned with a Coninue response.
   Determines how results from the ELISP are passed back
   to the server. Valid values are `ruby' and `rel'.

ELISP is any LISP expression

When the status is = the expression is returned to the invoking
elisp. When the status is * the ELISP is evaluated and the result is
fed back into Ruby.


From Elisp to Ruby

From elisp to ruby the data is:

HASH

Where supported keys are:

c  Command to run. Defaults to Run
t  Transaction to use. Only useful replying to a Continue.

=end

# basic class structure
class Rel
  class Server
    class RubyEvalError < StandardError; end
    class InternalError < StandardError; end
    class PacketError < InternalError; end
    class CommandError < InternalError; end
    class TransactionError < InternalError; end
    class Log
      def self.info(x)
        STDERR.puts x
      end
      def self.exception(e)
        STDERR.puts "<<ERROR>> " + e.message +
          "\n\t" + e.backtrace.join("\n\t")
      end
    end
  end
end

Log = Rel::Server::Log # XXX quick hack



require 'el4rim_objects'
Rel::CoreExt.import_core_extenions

#
# Keeps track of all information for a particular elisp-ruby
# "binding". Continuations are used to allow elisp callbacks.
#
class Rel::Server::Transaction
  RubyEvalError = Rel::Server::RubyEvalError

  def initialize
    @co = nil    # continuation
    @elisp = ""  # contains elisp to run if creating a continuation
    # not implemented, will be used to request how data comes back.
    @data_format = :native
  end

  # *** Methods to be called from inside a running transaction
  
  # Call an elisp function by issuing a Continue. +fn+ can be a string
  # or symbol. +args+ have +to_elisp+ invoked on them.
  def call (fn, *args)
    elisp([fn.to_sym, *args])
  end

  # Run some elisp code by issuing a Continue. +to_elisp+ is invoked
  # on +elisp+ which may not be desired. See +elisp!+
  def elisp (elisp)
    @elisp = elisp.to_elisp
    _make_continuation
  end

  # Like +elisp+ but automatically protects the object passed in which
  # is useful for passing an arbitrary string of elisp to eval. Make
  # sure to call to_elisp if required.
  def elisp! (elisp)
    @elisp = Rel::ElispExpression.new(elisp)
    _make_continuation
  end


  # *** Internal methods

  # Runs a ruby expression.
  def _run (expr)
    _ruby_eval expr
  end

  def _resume (expr)    
    value = _ruby_eval expr
    @co.call(value)
  end

  def _make_continuation
    val = callcc {|@co|
      throw :calling_elisp
    }
    @co = nil
    val
  end

  def _pending_elisp
    @elisp
  end

  def _calling_elisp?
    @co and true
  end

  def _ruby_eval (source)
    instance_eval(source)
    # other layers do all the work now
  rescue Exception => e
    Log.exception e
    raise RubyEvalError, e.class.to_s + ":" + e.message, e.backtrace
  end

  def _id; object_id; end

end


class Rel::Server
  TO_RUBY_CHUNK_SEP = "\n"
  FROM_RUBY_CHUNK_SEP = "\n"
  # commands
  RUN = "run"
  ECHO = "echo"
  VERSION = "version"

  def initialize (io_in, io_out, io_log)
    @in = io_in
    @out = io_out
    @tm = TransactionManager.new
  end

  def start
    Log.info "Server ready: #{Time.now}"
    while (input = read)
      response =
        begin
          request = Request.new(input)
          request.command ||= RUN
          Log.info "Recieved data:\n #{request.inspect}"
          handle_request(request)
        rescue RubyEvalError => e
          Log.exception e
          Response.new(Response::ERROR, e)
        rescue InternalError => e
          # invalid packet, etc.
          Log.exception e
          Response.new(Response::INTERNAL_ERROR, e)
        rescue Exception => e
          # oops, something very bad happened. should not be.
          Log.exception e
          Response.new(Response::INTERNAL_ERROR, InternalError.new(e.message))
        end
      write response.to_s
    end
    Log.info "Input stream closed. Exiting."
  end
  
  def handle_request (request)
    case request.command
    when RUN
      transaction =
        if transaction_id = request.options[:transaction]
          @tm.find(transaction_id)
        else
          @tm.new_transaction
        end

      Log.info("Using transaction: #{transaction._id}")

      response = do_run_command(transaction, request.data)
      if Response::CONTINUE == response.status
        response.options[:transaction] = transaction._id
        @tm.remember(transaction)
      else
        @tm.forget(transaction)
      end
      response
    when ECHO
      ## XXX doesn't currently "echo"
      Response.new(Response::RAW, "FAKE ECHO!")
    else
      raise CommandError, "Unsupported command '#{request.command}'"
    end
  end
  
  def do_run_command (transaction, ruby_code)
    result = catch :calling_elisp do
      if transaction._calling_elisp?
        transaction._resume(ruby_code)
      else
        transaction._run(ruby_code)
      end
    end
    # we can't just check result because it may be nil/false
    unless transaction._calling_elisp?
      Response.new(Response::FINISHED, result)
    else
      Response.new(Response::CONTINUE, transaction._pending_elisp)
    end
  end

  def write (chunk)
    @out.print chunk + FROM_RUBY_CHUNK_SEP
  end

  def read
    chunk = @in.gets(TO_RUBY_CHUNK_SEP)
    unless chunk.nil? # EOF?
      chunk[0..-(TO_RUBY_CHUNK_SEP.length + 1)]
    end
  end

end

# XXX - this should be made more general, like using a ruby Hash expr
class Rel::Server::Request
  attr_accessor :command, :options, :data

  PacketError = Rel::Server::PacketError

  def initialize (chunk)
    unless chunk =~ /^#,.*?#/
      raise PacketError, "Invalid packet '#{chunk[0,30]}..."
    end
    self.options = {}
    args = {}
    arg_str, raw_data = chunk[2..-1].split("#", 2)
    arg_str.split("/").collect do |arg|
      arg.split("=", 2)
    end.each do |k, v|
      args[k] = v
    end
    self.command = args["c"]
    self.data = raw_data
    self.options = args
    self.options[:transaction] = self.options.delete(:t)
  end
end

class Rel::Server::Response
  attr_accessor :status, :options, :data

  RAW = :"'"
  FINISHED = :"="
  CONTINUE = :"*"
  ERROR = :"!"
  INTERNAL_ERROR = :"!!"

  def initialize (status, data="")
    self.status = status
    self.data = data
    self.options = {}
  end

  # Creates 
  def response_chunk
    if options[:transaction]
      options[:t] = options.delete(:transaction)
      options[:d] = options.delete(:data_structure)
    end
    [status, options, data].to_elisp.to_s
  end
  alias_method :to_s, :response_chunk
end

class Rel::Server::TransactionManager
  Transaction = Rel::Server::Transaction
  TransactionError = Rel::Server::TransactionError

  def initialize
    @t = {} # transaction storage
  end
  def new_transaction
    Transaction.new
  end
  def find (transaction_id)
    @t[transaction_id.to_s] or
      raise(TransactionError, "Transaction '{transaction_id}' not found")
  end
  def remember (transaction)
    @t[transaction._id.to_s] = transaction
  end
  def forget (transaction)
    @t.delete(transaction._id.to_s)
  end
end


# doesn't return until server is finished
def rel_init
  if ENV.has_key? "LOG_TO_CONSOLE"
    puts "Logging to console"
  else
    log = File.open("/home/pstickne/el4rlog", "w+")
    log.sync = true
    STDERR.reopen(log)
  end

  STDOUT.sync = true
  # require 'stringio'
  # prevent side-effects
  #  io_out = ::STDOUT
  #  Object.class_eval {remove_const :STDOUT}
  #  ::STDOUT = StringIO.new
  
  $server = Rel::Server.new(STDIN, STDOUT, STDERR)
  $server.start
end


if __FILE__ == $0
  rel_init
end
