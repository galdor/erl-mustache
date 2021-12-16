#!/usr/bin/env ruby

require("json")

if ARGV.length() < 1
  $stderr.puts("Usage: #{$0} <spec-file>")
  exit(1)
end

file = ARGV[0]
mod_name = File.basename(file, ".json")

data = JSON.parse(IO.read(file))
tests = data["tests"]

def ruby_value_to_erlang_term(value, convert_hash_keys: false)
  if value.is_a?(Hash)
    entries = value.map do |k, v|
      k2 = convert_hash_keys ? ruby_value_to_erlang_term(k) : k
      "#{k2} => #{ruby_value_to_erlang_term(v)}"
    end
    return "\#{" + entries.join(", ") + "}"
  elsif value.is_a?(Array) && value.length() == 2 && value[0] == :raw
    return value[1]
  elsif value.is_a?(Array) && value.length() == 4 && value[0] == :mfa
    return "fun #{value[1]}:#{value[2]}/#{value[3]}"
  elsif value.is_a?(Array) && value.length() > 1 && value[0] == :tuple
    values = value.drop(1).map{|e| ruby_value_to_erlang_term(e)}.join(", ")
    return "{#{values}}"
  elsif value.is_a?(Array)
    entries = value.map do |v|
      "#{ruby_value_to_erlang_term(v)}"
    end
    return "[" + entries.join(", ") + "]"
  elsif value.is_a?(String)
    return "<<\"" + escape(value) + "\">>"
  else
    return "#{value}"
  end
end

def escape(s)
  table = {
    "\"" => "\\\"", # " -> \"
    "\\" => "\\\\", # \ -> \\
    "\r" => "\\r",
    "\n" => "\\n",
  }

  s.gsub(/["\\\r\n]/, table)
end

puts <<EOF
%% Copyright (c) 2020 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(mustache_spec_#{mod_name}_test).

-include_lib("eunit/include/eunit.hrl").

#{mod_name}_spec_test_() ->
   [
EOF

tests.each_with_index() do |test, i|
  name = test["name"]
  context = test["data"]
  expected = test["expected"]
  template = test["template"]
  desc = test["desc"]

  context = ruby_value_to_erlang_term(context)

  options = {}
  if test.key? "partials"
    partials = test["partials"].transform_values {|v| [:tuple, :string, v]}
    partials = ruby_value_to_erlang_term(partials, convert_hash_keys: true)

    options[:template_sources] = [:raw, partials]
  end
  options = ruby_value_to_erlang_term(options)

  puts  "    %% #{desc}"
  puts  "    ?_assertEqual({ok, <<\"#{escape(expected)}\">>},"
  puts  "                  render(<<\"#{escape(template)}\">>,"
  puts  "                         #{context},"
  print "                         #{options}))"

  if i < tests.length() - 1
    puts ","
  else
    puts ""
  end
end

puts <<EOF
  ].

-spec render(binary(), mustache:context(), mustache:options()) ->
        {ok, binary()} | {error, mustache:error()}.
render(TemplateString, Context, Options0) ->
  Options = Options0\#{return_binary => true},
  mustache:render(<<"test">>, {string, TemplateString}, Context, Options).
EOF
