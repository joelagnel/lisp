%%
%% Copyright (C) 2006  Tamas Patrovics
%%
%% This file is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2, or (at your option)
%% any later version.
%%
%% This file is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with GNU Emacs; see the file COPYING.  If not, write to
%% the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
%% Boston, MA 02111-1307, USA.
%%

-module(esense).

-include_lib("kernel/include/file.hrl").

-export([start/0]).
-export([parse_html_file/1]).
-export([parse_module_file/1]).
-export([parse_header_file/1]).
-export([get_errors/1]).
-export([parse_erlang_with_edoc/3]).

%----------------------------------------------------------------------

%% character used to separate file name components of include file
%% paths when generating include index files
-define(FILENAME_COMPONENT_SEPARATOR, $!).

-define(CHAR_TOGGLING_NON_ASCII, $$).

%----------------------------------------------------------------------

-record(content, {module_name, functions = [], records = [], macros = [],
                 includes = [], include_libs = [], imports = []}).

-record(function, {
          %% name of the function
          name,
          %% arity specification
          arity,
          %% function clause specification with argument names
          spec,
          %% HTML documentation reference
          docref,
          line, doc, params = [], 
          %% in case of the erlang module all functions are
          %% considered exported and the exported flag indicates
          %% if the function is auto-imported
          exported}).

-record(record, {name, line, doc, fields = []}).

-record(field, {name, doc}).

-record(macro, {name, value}).

-record(import, {module, functions}).

-record(imported_function, {name, arity}).

%----------------------------------------------------------------------
%
% Parse HTML documentation
%
parse_html({h3, _, "MODULE"}, {_, Content}) ->
    {module_name, Content};

parse_html({'div', _, Text},  {module_name, Content}) ->
    ModuleName = string:strip(remove_newlines(string:substr(Text, 2, length(Text) - 2))),
    %% some module names contain soft hyphen characters (ASCII 173)
    %% these are removed before storing the module name
    {next_function, 
     Content#content{module_name = [ C || C <- ModuleName, C /= 173 ]}};

parse_html({a, Attrs, Body}, {State, Content})
    when State == next_function; State == next_function_or_doc ->
    case lists:keysearch(name, 1, Attrs) of
        false ->
            % skip this anchor
            {next_function, Content};
        {value, {name, DocRef}} ->
            case string:rchr(DocRef, $/) of
                0 ->
                    Name = undefined,
                    Arity = undefined;
                Pos ->
                    Name = string:left(DocRef, Pos - 1),
                    Arity = string:substr(DocRef, Pos + 1)
            end,

            {Functions, NextState} = 
                case Name of
                    undefined ->
                        %% if the function has no name then it is generated from
                        %% a link which is not a function header, so skip it
                        {Content#content.functions, next_function};
                    _ ->
                        {[#function{name = Name, arity = Arity, docref = DocRef} 
                          | Content#content.functions],
                         function_strong}
                end,

            parse_html(Body, {NextState, Content#content{functions = Functions}})
    end;

parse_html({strong, _, Body}, {function_strong, Content}) ->
    parse_html(Body, {function_code, Content});

parse_html({code, _, Text}, {function_code, Content}) ->
    Header = resolve_character_entities(
               %% remove module name from function name (in module Erlang)
               %% and determined exported status of function
               %% In module Erlang all functions are considered exported
               %% and the exported flag indicates if the function is auto-imported.
               case string:str(Text, ":") of
                   0 ->
                       Exported = true,
                       Text;
                   Colon ->
                       %% if we matched a colon after a paren then
                       %% it wasn't a module name
                       case string:str(string:substr(Text, 1, Colon - 1), "(") of
                           0 ->
                               Exported = not (Content#content.module_name == "erlang"),
                               string:substr(Text, Colon + 1);
                           _ ->
                               Exported = true,
                               Text
                       end
                       
               end),

    [Function | Rest] = Content#content.functions,

    FuncWithSpec = Function#function{spec = normalize_string(Header),
                                     exported = Exported},

    {next_function_or_doc, Content#content{functions = [FuncWithSpec | Rest ]}};

% reset state if CODE tag is not matched in function_code state
parse_html(_, {function_code, Content}) ->
    {next_function, Content};

parse_html({'div', Attrs, Body}, {next_function_or_doc, Content}) ->
    case lists:keysearch(class, 1, Attrs) of
        {value, {class, "REFBODY"}} ->
            {NewState, NewContent} = 
                parse_html_body(Body, {function_doc_paragraph, Content}),

            [LastFunc | Rest] = NewContent#content.functions,
            case  LastFunc#function.spec of
                undefined ->
                    %% if the latest function has no specification then it is generated from
                    %% a link which is not a function header, so remove it
                    FixedFunctions = Rest;
                _ ->
                    FixedFunctions = copy_common_doc(NewContent#content.functions)
            end,

            FixedContent = Content#content{functions = FixedFunctions},

            case NewState of
                next_function_or_doc ->
                    {next_function_or_doc, FixedContent};
                _ ->
                    {next_function, FixedContent}
            end;
        _ ->
            {next_function_or_doc, Content}
    end;


parse_html({p, _}, {function_doc_paragraph, Content}) ->
    {function_doc_text, Content};

parse_html({p, _}, {function_doc_text, Content}) ->
    {next_function, Content};

parse_html({code, _, Text}, {function_doc_text, Content}) ->
    [LastFunc | Rest] = Content#content.functions,
    NewFunc = doc_append(LastFunc, 
                         string:concat(string:concat("<CODE>", 
                                                     Text),
                                       "</CODE>")),
    {function_doc_text, Content#content{functions = [NewFunc | Rest]}};

parse_html({a, _, Body}, {function_doc_text, Content}) ->
    parse_html(Body, {function_doc_text, Content});

parse_html({_, _, _}, {function_doc_text, Content}) ->
    {function_doc_text, Content};                  % FIXME: skip it for now

parse_html({_, _}, {function_doc_text, Content}) ->
    {function_doc_text, Content};                  % FIXME: skip it for now

parse_html(Text, {function_doc_text, Content}) ->
    case string:str(Text, "Types:") of
        0 ->
            [LastFunc | Rest] = Content#content.functions,
            NewFunc = doc_append(LastFunc, Text),
            {function_doc_text, Content#content{functions = [NewFunc | Rest]}};
        _ ->
            
            {function_doc_params, Content}
    end;


parse_html({'div', _, Body}, {function_doc_params, Content}) ->
    {_, NewContent} = parse_html_body(Body, {function_doc_params, Content}),
    {next_function_or_doc, NewContent};

parse_html({code, _, Text}, {function_doc_params, Content}) ->
    [Func | Rest] = Content#content.functions,
    Params = Func#function.params ++ [resolve_character_entities(normalize_string(Text))],
    NewFunc = Func#function{params = Params},
    {function_doc_params, Content#content{functions = [NewFunc | Rest]}};


parse_html({_, _, Body}, {State, Content}) ->
    parse_html_body(Body, {State, Content});

parse_html(_, {State, Content}) ->
    {State, Content}.

%----------------------------------------------------------------------
%
% Parse a YAWS HTML body element
%
parse_html_body(Body, {State, Content}) ->
    case is_list(Body) of
        true ->
            lists:foldl(fun(Child, {InnerState, InnerContent}) ->
                                parse_html(Child, {InnerState, InnerContent})
                        end,
                        {State, Content}, Body);
        _ ->
            parse_html(Body, {State, Content})
    end.
    
%----------------------------------------------------------------------
%
% Append Text to the documentation of function
%
doc_append(Func, Text) ->
    case Func#function.doc of
        undefined ->
            OldDoc = "";
        Doc ->
            OldDoc = Doc
    end,

    NewDoc = normalize_string(string:concat(string:concat(OldDoc, "\n"),
                                            normalize_string(Text))),

    Func#function{doc = NewDoc}.

%----------------------------------------------------------------------
%
% Copy documentation of latest function to all previous functions which
% has no documentation. These are functions which are documented 
% together.
% 
copy_common_doc(Content) ->
    [LastFunc | Previous ] = Content,

    case LastFunc#function.doc of
        undefined ->
            Content;
        Doc ->
            Params = LastFunc#function.params,

            %% add the same doc and params to all previous functions 
            %% which has no documentation
            FixedFunctions =
                lists:map(fun (Function) ->
                                  case  Function#function.doc of
                                      undefined ->
                                          Function#function{doc = Doc, params = Params};
                                      _ ->
                                          %% we should stop mapping here, but I don't
                                          %% know how
                                          Function
                                  end
                          end,
                          Previous),
            [LastFunc | FixedFunctions]
    end.

%----------------------------------------------------------------------

parse_erlang(Node, {Exports, PrevComments, Content}) ->
    case erl_syntax:type(Node) of
        form_list ->
            lists:foldl(fun parse_erlang/2, {Exports, PrevComments, Content},
                        erl_syntax:form_list_elements(Node));

        attribute ->
            Name = erl_syntax:attribute_name(Node),
            case erl_syntax:type(Name) of
                atom ->
                    case erl_syntax:atom_literal(Name) of
                        "export" ->
                            case Exports of
                                all ->
                                    {Exports, PrevComments, Content};
                                _ ->
                                    [ExportAttrs] = erl_syntax:attribute_arguments(Node),
                                    {Exports ++
                                     lists:map(fun (Export) ->
                                                       parse_export(Export)
                                               end,
                                               erl_syntax:list_elements(ExportAttrs)),
                                     [], Content}
                            end;
                        "module" ->
                            [Attribute] = erl_syntax:attribute_arguments(Node),
                            ModuleName = erl_syntax:atom_value(Attribute),
                            
                            %% module names having more than one
                            %% component are skipped for now
                            case is_list(ModuleName) of
                                true ->
                                    throw(skip_module);
                                _ ->
                                    {Exports, [], 
                                     Content#content{module_name = ModuleName}}
                            end;
                        "compile" ->
                            [Attribute] = erl_syntax:attribute_arguments(Node),
                            case erl_syntax:atom_value(Attribute) of
                                export_all ->
                                    {all, PrevComments, Content};
                                _ ->
                                    {Exports, PrevComments, Content}
                            end;
                        "import" ->
                            Imports = Content#content.imports ++ [parse_import(Node)],
                            {Exports, PrevComments, 
                             Content#content{imports = Imports}};
                        "record" ->
                            Record = parse_erlang_record(Node, PrevComments, Content),
                            Records = Content#content.records ++ [Record],
                            {Exports, [], Content#content{records = Records}};
                        "define" ->
                            Macro = parse_erlang_macro(Node),
                            Macros = Content#content.macros ++ [Macro],
                            {Exports, [], Content#content{macros = Macros}};
                        "include" ->
                            Includes = Content#content.includes ++
                                [parse_include(Node)],
                            {Exports, [], Content#content{includes = Includes}};
                        "include_lib" ->
                            Includes = Content#content.include_libs ++
                                [parse_include(Node)],
                            {Exports, [], Content#content{include_libs = Includes}};
                        _ ->
                            {Exports, [], Content}
                    end;
                _ ->
                    {Exports, [], Content}
            end;

        function ->
            Function = parse_erlang_function(Node, PrevComments),
            NameInfo = erl_syntax:function_name(Node),
            Name = erl_syntax:atom_value(NameInfo),
            Arity = erl_syntax:function_arity(Node),

            case Exports of
                all ->
                    Exported = true;
                _ ->
                    Exported = lists:member({Name, Arity}, Exports)
            end,

            Functions = Content#content.functions ++
                [Function#function{exported = Exported}],
            {Exports, [], Content#content{functions = Functions}};

        comment ->
            {Exports, format_comments(Node), Content};

        _ ->
            %%io:format("~p~n", [erl_syntax:type(Node)]),
            {Exports, [], Content}
    end.


parse_erlang_function(Node, PrevComments) ->
    NameInfo = erl_syntax:function_name(Node),
    Name = erl_syntax:atom_literal(NameInfo),
    Line = erl_syntax:get_pos(NameInfo),
    Arity = erl_syntax:function_arity(Node),

    case erl_syntax:get_precomments(Node) of
        [Comment] ->
            Doc = format_comments(Comment);
        _ ->
            case PrevComments of
                [] ->
                    Doc = undefined;
                _ ->
                    Doc = PrevComments
            end
    end,

    #function{name = Name,
              arity = Arity,
              line = Line,
              doc = Doc}.
    

parse_export(Export) ->
    {erl_syntax:atom_value(erl_syntax:arity_qualifier_body(Export)),
     erl_syntax:integer_value(erl_syntax:arity_qualifier_argument(Export))}.


parse_import(Node) ->
    [Module, Imports] = erl_syntax:attribute_arguments(Node),

    Functions = [#imported_function{name = 
                                    erl_syntax:atom_literal(
                                      erl_syntax:arity_qualifier_body(Import)),
                                    arity = 
                                    erl_syntax:integer_value(
                                      erl_syntax:arity_qualifier_argument(Import))}
                 || Import <- erl_syntax:list_elements(Imports)],

    #import{module = erl_syntax:atom_literal(Module), functions = Functions}.


parse_erlang_record(Node, PrevComments, Content) ->
    case erl_syntax:get_precomments(Node) of
        [] ->
            case PrevComments of
                "" ->
                    Doc = undefined;
                _ ->
                    Doc = PrevComments
            end;
        [Comment] ->
            Doc = format_comments(Comment)
    end,

    Record = lists:foldl(
               fun (Arg, InnerRecord) ->
                       case erl_syntax:type(Arg) of
                           atom ->
                               InnerRecord
                                   #record{name = erl_syntax:atom_name(Arg)};
                           tuple ->
                               parse_erlang_record_fields(
                                 erl_syntax:tuple_elements(Arg),
                                 InnerRecord);
                           macro ->
                               MacroName = erl_syntax:variable_literal(
                                             erl_syntax:macro_name(Arg)), 

                               Result = lists:filter(fun(Macro) -> 
                                                             Macro#macro.name == MacroName
                                                     end,
                                                     Content#content.macros),

                               case Result of 
                                   [] -> 
                                       Name = "name_cannot_be_resolved"; 
                                   [Macro] -> 
                                       Name = Macro#macro.value 
                               end,

                               InnerRecord #record{name = Name}
                       end
               end,
               #record{},
               erl_syntax:attribute_arguments(Node)),

    Record#record{line = erl_syntax:get_pos(Node),
                  doc = Doc}.

parse_erlang_record_fields(Fields, Record) ->
    lists:foldl(
      fun (Field, InnerRecord) ->
              FieldName = erl_syntax:record_field_name(Field),

              Name = case erl_syntax:type(FieldName) of
                         atom ->
                             erl_syntax:atom_name(FieldName);
                         macro ->
                             %% FIXME
                             "Resolving_macro_record_fields_is_not_implemented"
                     end,

              case erl_syntax:get_postcomments(Field) of
                  [] ->
                      case erl_syntax:get_precomments(Field) of
                          [] ->
                              Comment = none;
                          [Comment|_] ->
                              ok
                      end;
                  [Comment] ->
                      ok
              end,

              case Comment of
                  none ->
                      Doc = undefined;
                  _ ->
                      Doc = join_with_newlines(                              
                              collapse_whitespace(
                                [string:strip(Line) ||
                                    Line <-
                                        erl_syntax:comment_text(Comment)]))
              end,

              InnerRecord#record{fields = InnerRecord#record.fields ++
                                 [#field{name = Name, doc = Doc}]}
      end,
      Record,
      Fields).


parse_erlang_macro(Node) ->
    [Define | Definition] = erl_syntax:attribute_arguments(Node),
    case erl_syntax:type(Define) of
        variable ->
            Macro = atom_to_list(erl_syntax:variable_name(Define)),
            Value = parse_erlang_macro_value(Definition);

        application ->
            Arguments = erl_syntax:application_arguments(Define),

            case Arguments of
                [OneArg] ->
                    case erl_syntax:type(OneArg) of
                        macro ->
                            %% it's a macro defined as an other
                            %% macro
                            ProcessArgs = false;
                        _ ->
                            ProcessArgs = true
                    end;
                _ ->
                    ProcessArgs = true
            end,

            case ProcessArgs of
                true ->
                    Args = lists:foldl(
                             fun (Arg, Acc) ->
                                     case Acc of
                                         "" ->
                                             NewAcc = Acc;
                                         _ ->
                                             NewAcc = Acc ++ ", "
                                     end,
                                     NewAcc ++ atom_to_list(erl_syntax:variable_name(Arg))
                             end,
                             "",
                             Arguments);
                _ ->
                    Args = undefined
            end,

            Operator = erl_syntax:application_operator(Define),
            case erl_syntax:type(Operator) of
                variable ->
                    MacroName = atom_to_list(erl_syntax:variable_name(Operator));
                atom ->
                    MacroName = erl_syntax:atom_literal(Operator)
            end,
            

            Macro = MacroName ++
                case Args of
                    undefined ->
                        "";
                    _ ->
                        "(" ++ Args ++ ")"
                end,

            Value = undefined;

        atom ->
            % possible single quotes are stripped from the
            % beginning and end of macro name
            Macro = string:strip(erl_syntax:atom_literal(Define),
                                 both, 39),
            Value = parse_erlang_macro_value(Definition)

    end,
    #macro{name = Macro, value = Value}.


parse_erlang_macro_value([Definition]) ->
    case erl_syntax:type(Definition) of
        integer ->
            %% integers are converted to strings to avoid problems
            %% with big numbers if the parsed info is sent to Distel
            integer_to_list(erl_syntax:integer_value(Definition));
        atom ->
            erl_syntax:atom_name(Definition);
        string ->
            erl_syntax:string_literal(Definition);
        char ->
            erl_syntax:char_literal(Definition);
       macro ->
            Name = erl_syntax:macro_name(Definition),
            "?" ++ 
                case erl_syntax:type(Name) of
                    atom ->
                        erl_syntax:atom_name(Name);
                    variable ->
                        erl_syntax:variable_literal(Name)
            end;
        _ ->
            undefined
    end;
parse_erlang_macro_value(_) ->
    undefined.


parse_include(Node) ->
    [Arg] = erl_syntax:attribute_arguments(Node),
    erl_syntax:string_value(Arg).


%----------------------------------------------------------------------

parse_path(Path) ->
    case file:read_file_info(Path) of
        {error, _} ->
            1;
        {ok, Fileinfo} ->
            case Fileinfo#file_info.access of
                none ->
                    io:format("File not readable: ~s~n", [Path]),
                    1;
                _ ->
                    case file:read_link(Path) of
                        {ok, _} ->
                            0;
                        _ ->
                            case filelib:is_dir(Path) of
                                true ->
                                    parse_dir(Path);
                                false ->
                                    parse_file(Path)
                            end
                    end
            end
    end.


parse_dir(Path) ->
    {ok, Files} = file:list_dir(Path),
    lists:foreach(fun(File) ->
                          parse_path(filename:join(Path, File))
                  end,
                  Files),
    0.                                          % exit status

parse_file(Path) ->
    case get(debug) of
        undefined ->
            skip;
        _ ->
            io:format("Processing file ~s~n", [Path])
    end,

    case filename:extension(Path) of
        ".html" ->
            case catch parse_html_file(Path) of
                {'EXIT', R} ->
                    io:format("Error when reading file: ~s~n~p~n", [Path, R]),
                    1;                          % exit status
                Content ->
                    generate_module(Content, Path),
                    0
            end;
        ".erl" ->
            case parse_module_file(Path) of
                skip_module ->
                    io:format("Skipping module: ~s~n", [Path]),
                    0;                
                Content ->
                    generate_module(Content, Path),
                    0                   % exit status
            end;
        ".hrl" ->
            Content = parse_header_file(Path),
            generate_header(Content, Path),
            0;                   % exit status
        _ ->
            0
    end.


parse_html_file(Path) ->
    {_, Content} = parse_html(yaws_html:parse(Path), {none, #content{}}),
    Content.


parse_module_file(Path) ->
    {ok, Source} = epp_dodger:parse_file(Path),
    Comments = erl_comment_scan:file(Path),
    ParseTree = erl_recomment:recomment_forms(Source, Comments),
    {_, _, Content} = parse_erlang(ParseTree, {[], [], #content{}}),
    EdocInfo = parse_erlang_with_edoc(Source, Comments, Path),
    merge_with_edoc(Content, EdocInfo).

parse_header_file(Path) ->
    {ok, Source} = epp_dodger:parse_file(Path),
    Comments = erl_comment_scan:file(Path),
    {_, _, Content} = parse_erlang(erl_recomment:
                                   recomment_forms(Source, Comments),
                                   {[], [], #content{}}),
    Content.    

get_errors(Path) ->
    {ok, Forms} = epp_dodger:parse_file(Path),
    [begin
         {ErrorLine, Module, ErrorDescriptor} =
             erl_syntax:error_marker_info(Form),
         ErrStr = lists:flatten(io_lib:fwrite("~s", [apply(Module, 
                                                           format_error, 
                                                           [ErrorDescriptor])])),
         {ErrorLine, ErrStr}
     end
     || Form <- Forms, erl_syntax:type(Form) == error_marker].

%----------------------------------------------------------------------

generate_module(Content, Path) ->
    ModuleName = Content#content.module_name,
    case Content#content.functions of
        [] ->
            io:format("Module doesn't have any functions. Skipping: ~s~n", 
                      [Path]);
        _ ->
            case get(dump_to_stdout) of
                undefined ->
                    io:format("Generating index for module ~s~n", [ModuleName]),

                    ModuleDir = filename:join(get(cache_dir),
                                              "modules"),
                    ensure_dir(ModuleDir),

                    {ok, File} = file:open(filename:join(ModuleDir, ModuleName),
                                           [write]),

                    io:format(File, "~s~n", [Path]);
                _ ->
                    File = undefined,
                    io:format("~s~n", [Path])
            end,

            generate_index(Content, File)
    end.


generate_header(Content, Path) ->
    BaseName = filename:basename(Path),

    case get(dump_to_stdout) of
        undefined ->
            io:format("Generating index for header file ~s~n", [BaseName]),

            HeaderDir = filename:join(get(cache_dir), "includes"),
            ensure_dir(HeaderDir),

            AllComponents = filename:split(Path),
            %% assert it's absolute
            [First | Components] = AllComponents,
            case filename:pathtype(First) of
                absolute ->
                    ok
            end,

            case os:type() of
                {win32, _} ->
                    [Drive, $:|Rest] = First,
                    IndexFileName =
                        join_with_char(lists:reverse([[Drive, $_|Rest]|Components]),
                                       ?FILENAME_COMPONENT_SEPARATOR);
                _ ->
                    IndexFileName =
                        join_with_char(lists:reverse(Components),
                                       ?FILENAME_COMPONENT_SEPARATOR)
            end,

            {ok, File} = file:open(filename:join(HeaderDir, IndexFileName),
                                   [write]),
            io:format(File, "~s~n", [Path]);
        _ ->
            File = undefined,
            io:format("~s~n", [Path])            
    end,

    generate_index(Content, File).


generate_index(Content, File) ->
    case get(full_index) of
        undefined ->
            skip;
        _ ->
            lists:foreach(fun (Import) ->
                                  generate_import(Import, File)
                          end,
                          Content#content.imports),

            lists:foreach(fun (Include) ->
                                  write_data("include", Include, File)
                          end,
                          Content#content.includes),

            lists:foreach(fun (Include) ->
                                  write_data("includelib", Include, File)
                          end,
                          Content#content.include_libs),

            lists:foreach(fun(Function) -> 
                                  generate_function(Function, File)
                          end,
                          Content#content.functions),

            lists:foreach(fun (Record) ->
                                  generate_record(Record, File)
                          end,
                          Content#content.records),

            lists:foreach(fun (Macro) ->
                                  generate_macro(Macro, File)
                          end,
                          Content#content.macros)
    end,

    case File of
        undefined ->
            skip;
        _ ->
            file:close(File)
    end.


generate_import(Import, File) ->
    write_data("import", Import#import.module, File),
    lists:foreach(fun (Function) ->
                          write_data("name", Function#imported_function.name, File),
                          write_data("arity", Function#imported_function.arity, File)
                  end,
                  Import#import.functions).


generate_function(Function, File) ->
    write_data("function", Function#function.name, File),
    write_data("arity", Function#function.arity, File),
    write_data("spec", Function#function.spec, File),
    write_data("docref", Function#function.docref, File),
    write_data("line", Function#function.line, File),

    lists:foreach(fun(Param) ->
                          write_data("param", Param, File)
                  end,
                  Function#function.params),

    write_data("doc", Function#function.doc, File),

    case Function#function.exported of
        true ->
            write_data("exported", "true", File);
        _ ->
            skip
    end.
    

generate_record(Record, File) ->
    write_data("record", Record#record.name, File),
    write_data("line", Record#record.line, File),
    write_data("doc", Record#record.doc, File),
    lists:foreach(fun(Field) ->
                          write_data(field, Field#field.name, File),
                          write_data(doc, Field#field.doc, File)
                  end,
                  Record#record.fields).


generate_macro(Macro, File) ->
    write_data("macro", Macro#macro.name, File),
    case Macro#macro.value of
        undefined ->
            skip;
        _ ->
            write_data("value", Macro#macro.value, File)
    end.
            

write_data(_, undefined, _) ->
    skip;

write_data(Name, Value, File) when integer(Value) ->
    write_data(Name, integer_to_list(Value), File);

write_data(Name, Value, File) ->
    %% if doc string has newlines in it then add
    %% special terminator char around the string    
    case string:chr(Value, 10) of
        0 ->
            FinalValue = Value;
        _ ->
            FinalValue = lists:concat([[30], Value, [30]])
    end,

    case File of
        undefined ->
            io:format("~s:~s~n", [Name, FinalValue]);
        _ ->
            io:format(File, "~s:~s~n", [Name, FinalValue])
    end.

%----------------------------------------------------------------------

usage() ->
    io:format("~n"),
    io:format("    esense [<options>] <file/directory> [<cache directory>]~n"),
    io:format("    esense [<options>] -files-from-stdin [<cache directory>]~n"),
    io:format("~n"),
    io:format("Creates index files from Erlang source files or from the official~n"),
    io:format("HTML documentation (not from the ones generated with edoc).~n"),
    io:format("~n"),
    io:format("Arguments:~n"),
    io:format("~n"),
    io:format("    <file/directory>   If a file is given then the index is generated from it.~n"),
    io:format("                       In case of a directory all files from that directory are processed.~n"),
    io:format("                       Symbolic links are not followed.~n"),
    io:format("    <cache directory>  Directory where the index files are written.~n"),
    io:format("                       If not given it defaults to $HOME/.esense~n"),
    io:format("~n"),
    io:format("Options:~n"),
    io:format("~n"),
    io:format("    -full              Generate full index instead of just a stub.~n"),
    io:format("                       (Normally you don't need to use this option.)~n"),
    io:format("    -stdout            Dump indexing result to the standard output~n"),
    io:format("                       instead of writing it into the result file.~n"),
    io:format("    -debug             Write file names as they are processed to the~n"),
    io:format("                       standard output. This helps identifying the~n"),
    io:format("                       problematic file if ESense crashes during indexing.~n"),
    io:format("    -files-from-stdin  Read input file list from the standard input.~n"),
    io:format("~n"),

    halt(1).

%----------------------------------------------------------------------

process_options([]) ->
    [];

process_options([[First | Option] | Rest] = Remainder) ->
    case First of
        $- ->
            case Option of
                "full" ->
                    put(full_index, true);
                "stdout" ->
                    put(dump_to_stdout, true);
                "debug" ->
                    put(debug, true);
                "files-from-stdin" ->
                    put(files_from_stdin, true);
                _ ->
                    usage()
            end,
            process_options(Rest);
        _ ->
            Remainder
    end.

%----------------------------------------------------------------------

process_commandline(AllArgs) ->
    Args = process_options(AllArgs),

    put(cache_dir, filename:join(os:getenv("HOME"), ".esense")), %default

    case {length(Args), get(files_from_stdin)/=undefined} of
        {0,false} ->
            usage();
        {0,true} ->
            get_filelist_from_stdin();
        {1,false} ->
            [lists:nth(1, Args)];
        {1,true} ->
            %% we've specified $0 -files_from_stdin <file>
            %% <file> is the cache dir
            put(cache_dir, lists:nth(1, Args)),
            get_filelist_from_stdin();
        {2,false} ->
            put(cache_dir, lists:nth(2, Args)),
            [lists:nth(1, Args)];
        {2,true} ->
            %% $0 -files_from_stdin <file1> <file2># doesn't make any sense
            usage();
        _ ->
            usage()
    end.

%----------------------------------------------------------------------

get_filelist_from_stdin() ->
    case io:get_line("") of
        eof ->
            [];
        Chars ->
            %% newlines are removed from end of line
            [string:strip(Chars, right, 10) |  get_filelist_from_stdin()]
    end.

%----------------------------------------------------------------------

start() ->
    Args =  init:get_plain_arguments(),
    if
        Args == [] ->
            usage();
        true ->
            Inputs = process_commandline(Args),
            filelib:ensure_dir(filename:join(get(cache_dir), "dummy")),
            lists:foreach(fun (File) ->
                                  Result = parse_path(absname(File)),
                                  case Result of
                                      0 ->
                                          skip;
                                      _ ->
                                          halt(Result)
                                  end
                          
                          end,
                          Inputs),
            halt(0)
    end.
        
%----------------------------------------------------------------------

normalize_string(Str) ->
    string:strip(collapse_whitespace(remove_newlines(Str))).

%----------------------------------------------------------------------

resolve_character_entities(Str) ->
    lists:foldl(fun ({Entity, Replacement}, InnerStr) ->
                        case string:str(Str, Entity) of
                            0 ->
                                InnerStr;
                            _ ->
                                {ok, NewStr, _} = regexp:sub(InnerStr, Entity, Replacement),
                                NewStr
                        end
                end,
                Str,
                [{"&#60;", "<"}, {"&#62;", ">"}]).

%----------------------------------------------------------------------

join_with_char([E] = L, _) when length(L) == 1 ->
    E;
join_with_char([H|T], C) ->
    H ++ [C] ++ join_with_char(T, C).

%----------------------------------------------------------------------

join_with_newlines(L) ->
    join_with_char(L, $\n).

%----------------------------------------------------------------------

%% change newlines to one space or omit it if no space is needed
remove_newlines([], _) -> 
    [];
remove_newlines([10 | T], PrevChar) -> 
    case PrevChar of
        $( ->
                 remove_newlines(T, $();                                 
         _ ->
                 case T  of
                     [NextChar | _] ->
                         if 
                             NextChar == $. ; NextChar == $) ; NextChar == $, ->
                                  remove_newlines(T, PrevChar);
                             true ->
                                  remove_newlines([32 | T], 32)
                         end;
                     _ ->
                         remove_newlines(T, 32)
                  end
         end;
remove_newlines([H|T], _) -> 
    [H|remove_newlines(T, H)].


remove_newlines(Str) -> 
    remove_newlines(Str, a).

%----------------------------------------------------------------------

collapse_whitespace([], _) ->
    [];
collapse_whitespace([$ |Rest], $ ) ->
    collapse_whitespace(Rest, $ );
collapse_whitespace([FirstChar|Rest], _) ->
    [FirstChar | collapse_whitespace(Rest, FirstChar)].

collapse_whitespace(Str) ->
    collapse_whitespace(Str, none).

%----------------------------------------------------------------------

ensure_dir(Dir) ->
    filelib:ensure_dir(filename:join(Dir, "dummy")).

%----------------------------------------------------------------------

absname(File) ->
    Components = filename:split(filename:absname(File)),
    Filtered = lists:foldl(fun("..", [RootDir]) -> 
                                   [RootDir];
                              ("..", [_PrevDir|Rest]) ->
                                   Rest; 
                              (Component, PathSoFar) -> 
                                   [Component|PathSoFar]
                           end, [], Components),
    filename:join(lists:reverse(Filtered)).

%----------------------------------------------------------------------

remove_percent_chars([$% | Cs]) -> 
    [$\s | remove_percent_chars(Cs)];

remove_percent_chars(Cs) -> 
    Cs.

%----------------------------------------------------------------------

format_comments(Node) ->
    join_with_newlines([remove_percent_chars(S) || 
                           S <- erl_syntax:comment_text(Node)]).

%----------------------------------------------------------------------

%% Edoc handling (contributed by Andrey Grigoriev)

merge_with_edoc(ModuleInfo, undefined) ->
    ModuleInfo;
merge_with_edoc(ModuleInfo, []) ->
    ModuleInfo;
merge_with_edoc(ModuleInfo, EdocInfo) ->
    Functions = ModuleInfo#content.functions,
    ActualFunctions = [get_actual_function(F, EdocInfo) || F <- Functions],
    ModuleInfo#content{functions = ActualFunctions}.

get_actual_function(#function{name = Name, arity = Arity} = F, EdocInfo) ->
    case lists:keysearch(lists:concat([Name, "/", Arity]), 1, EdocInfo) of
        {value, EdocFunc} ->
            compose_function(F, EdocFunc);
        _Other ->
            F
    end.

compose_function(F, {_Name, {{FullName, private}, Params}, Doc}) ->
    F#function{spec = FullName,
               doc = from_nonascii_bypass(Doc),
               exported = false,
               params = Params};
compose_function(F, {_Name, {FullName, Params}, Doc}) ->
    F#function{spec = FullName,
               doc = from_nonascii_bypass(Doc),
               exported = true,
               params = Params};
compose_function(F, _Other) ->
    F.

parse_erlang_with_edoc(Source, Comments, Filename) ->
    %% FIXME: Convert the file to UTF-8 before parsing to ensure that
    %% non-ascii symbols won't break edoc
    CommentsToParse = guard_nonascii_comments(Comments),
    case catch erl_recomment:recomment_forms(Source, CommentsToParse) of
        {'EXIT', _} ->
            undefined;
        ParseTree ->
            %% FIXME: The definition of macro @vsn added to ensure that edoc
            %% itself will be processed
            %%    case catch edoc:get_doc(FileToParse, [{def, {'vsn', "current"}}]) of
            case catch edoc_extract:source(ParseTree, Filename, 
                                            edoc_lib:get_doc_env([{def, {'vsn', "current"}}]),
                                            [{def, {'vsn', "current"}}]) of
                {'EXIT', _Reason} ->
                    undefined;
                syntax_error ->
                    undefined;
                {_Module, Xml} ->
                    case catch esense_edoc_layout:layout_module(Xml) of
                        {ok, EdocInfo} ->
                            EdocInfo;
                        _Other ->
                            undefined
                    end
            end
    end.

guard_nonascii_comments(Comments) ->
    lists:map(fun({Line, Column, Indentation, Text}) ->
                      {Line, Column, Indentation, 
                       lists:map(fun to_nonascii_bypass/1, Text)}
              end, Comments).

%% Home-grown encoding scheme to make edoc not to fail on non-ascii
%% symbols. 
to_nonascii_bypass(S) ->
    lists:flatten(
      lists:map(fun(Ch) ->
                        if
                            Ch == ?CHAR_TOGGLING_NON_ASCII ->
                                [Ch, Ch];
                            Ch < 128 ->
                                Ch;
                            true ->
                                [?CHAR_TOGGLING_NON_ASCII,
                                 integer_to_list(Ch),
                                 ?CHAR_TOGGLING_NON_ASCII]
                        end
                end,
                S)).
                        
from_nonascii_bypass([]) ->                
    [];
from_nonascii_bypass([?CHAR_TOGGLING_NON_ASCII, ?CHAR_TOGGLING_NON_ASCII|Rest]) ->
    [?CHAR_TOGGLING_NON_ASCII|from_nonascii_bypass(Rest)];
from_nonascii_bypass([?CHAR_TOGGLING_NON_ASCII|Rest]) ->
    {Int, Rest1} = get_int_part(Rest),
    [list_to_integer(Int)|from_nonascii_bypass(Rest1)];
from_nonascii_bypass([C|Rest]) ->
    [C|from_nonascii_bypass(Rest)].

get_int_part(List) ->
    get_int_part(List, []).

get_int_part([C|Rest], L) ->
    if
        C >= $0, C =< $9 ->
            get_int_part(Rest, [C|L]);
        C == ?CHAR_TOGGLING_NON_ASCII ->
            {lists:reverse(L), Rest}
    end. 
