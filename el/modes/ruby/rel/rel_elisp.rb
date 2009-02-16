#
# Paul Nathan Stickney <pstickne@gmail.com>
#
#

#
# +to_elisp+ support for Obects.
#
# +to_elisp+ _must_ take one argument, a hash which is 
# used to pass around the state and options.
#
# Minimum supported keys are:
#
#   :quote => true/false
#     true - quote this if not :in_quote, do not propagate quoting (remove from hash)
#
#   :in_quote => true/false
#     Recursive data-structures should pass this to
#     children as true if they performed quoting or are in a quote
#     themselves. If :in_quote is true, :quote => true should be ignored.
#

if $ZENTEST
  class Rel; end
end

# A LISP expression is a LISP expression. This is used as a means to
# protect an object, such as one created via +to_elisp+ from being
# elisp'ified again.
class Rel::ElispExpression
  def initialize (value)
    @value = value
  end
  # we already ARE elisp code
  def to_elisp (state=nil)
    @value
  end
  def to_s
    @value.to_s
  end
end


#
# Core extensions to provide +to_elisp+
#
class Rel::CoreExt
  def self.import_core_extensions
    constants.each do |ext_name|
      klass = Object.const_get(ext_name)
      mod = Object.const_get("#{self}::#{ext_name}")
      klass.send(:include, mod)
    end
  end
end

# These are all individually prefixed due to
# a bug in zentest 3.4.0
class Object
  def to_elisp (state=nil)
    to_s.to_elisp
  end
end

class Exception
  def to_elisp (state=nil)
    [self.class, message, backtrace].to_elisp(state)
  end
end

class FalseClass
  def to_elisp (state=nil)
    Rel::ElispExpression.new("false")
  end
end

class NilClass
  def to_elisp (state=nil)
    Rel::ElispExpression.new("nil")
  end
end

# XXX - elisp can't handle larger numbers
class Numeric
  # a number is a number
  def to_elisp (state=nil)
    Rel::ElispExpression.new(self)
  end
end

class String
  def to_elisp (state=nil)
    Rel::ElispExpression.new(inspect)
  end
end

class Array
  def to_elisp (state={})
    start = if state[:quote] == true and not state[:in_quote]
              state.delete(:quote)
              state[:in_quote] = true
              "'("
            else
              "("
            end
    Rel::ElispExpression.new(start + elements_to_elisp(state) + ")")
  end
  def elements_to_elisp (state)
    collect do |v|
      v.to_elisp(state)
    end.join(" ")
  end
end

class Hash
  # Default is to turn it into an ALIST
  def to_elisp (state={})
    start = if state[:quote] == true and not state[:in_quote]
              state.delete(:quote)
              state[:in_quote] = true
              "'("
            else
              "("
            end
    Rel::ElispExpression.new(start + elements_to_elisp(state) + ")")
  end
  def elements_to_elisp (state)
    collect do |k, v|
      [k, Rel::ElispExpression.new("."), v].to_elisp(state)
    end.join(" ")
  end
end

class Symbol
  # :foo_bar -> foo-bar
  # :_foo_bar -> 'foo-bar
  def to_elisp (state=nil)
    Rel::ElispExpression.new(to_s.sub(/^_/, "'").gsub(/_/, '-'))
  end
end

