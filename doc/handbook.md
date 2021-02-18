% erl-mustache

# Introduction
The erl-mustache project is an implementation of the
[Mustache](https://mustache.github.io/) templating language.

# Conformance
Mustache is not clearly defined. The [official
manual](https://mustache.github.io/mustache.5.html) is extremely
imprecise. There is also a [specification
repository](https://github.com/mustache/spec) which is even less
clear. Finally, lots of Mustache implementation include features which may or
may not make then incompatible with each other.

In consequence, the erl-mustache application implements what is widely
considered as being part of Mustache, using the 1.1.3 of the specification
repository as base reference. Some limitations of the specification are
ignored (e.g. the limit of two characters for alternate delimiters) when doing
so does not impact the ability to render Mustache templates originally written
for other implementations. Extra language features are also made available
under the same conditions.

The test suite includes all mandatory tests from the specification repository.

# Interface
## Rendering
Templates can be rendered either from files or from strings. The following
variants of the rendering function are available:
- `mustache:render(Name, Context)`.
- `mustache:render(Name, Source, Context)`.
- `mustache:render(Name, Context, Options)`.
- `mustache:render(Name, Source, Context, Options)`.

The arguments being:
- `Name`: a binary identifying the template.
- `Source` the place where the content of the template is available,
  specified with one of the following expressions:
  - `{file, Path}`: a file; if the path is relative, it is concatenated to the
    template directory.
  - `{string, String}`: a literal binary.
  - `default`: either an entry of the `template_sources` option or a file
    whose path is based on the name of the template, with the `.mustache`
    extension, e.g. `blog/posts.mustache` for a template named `blog/posts`.
- `Context`: the set of data made available to the template.
- `Options`: the set of options used for parsing and generation.

Example:
```erlang
mustache:render(<<"hello">>, {string, <<"Hello {{name}}!">>},
                #{name => <<"Bob">>},
                #{return_binary => true}).
```

## Context
A context is a set of data used by the template when generating the final
document. During generation, values from the context are used for variable and
section substitutions. When a value must be inserted in the final document, it
is transformed into a string according to the following rules:
- If the value is of the form `{data, Data}` where `Data` is of type
  `iodata()`, the data lis is returned as it is.
- If the value is a binary, it is treated as a character string and returned
  as such.
- If not, it is pretty printed using the `~0tp` format control sequence (see
  the [Erlang documentation](https://erlang.org/doc/man/io.html).

For example:
```erlang
Users = [<<"Bob">>, <<"Alice">>, <<"Eve">>],
mustache:render(<<"hello">>,
                {string, <<"{{nb_users}} users:\n{{#users}}- {{.}}.\n{{/users}}">>},
                #{users => Users,
                  nb_users => length(Users)}).
```

Will yield the following document:
```
Users:
- Bob.
- Alice.
- Eve.
```

## Options
Options are represented as a map. The following options are available:
- `template_directory`: the path of the directory where templates are stored
  (default: `.`).
- `template_sources`: a map associating template names to template sources,
  used when the template source is `default` and for partials. For example:
  ```erlang
  mustache:render(<<"example">>,
                  {string, <<"Hello {{>a}}!">>},
                  #{name => <<"Alice">>},
                  #{template_sources =>
                      #{<<"a">> => {string, <<"{{name}}">>}},
                    return_binary => true}).
  ```
- `disable_html_escaping`: disable HTML escaping for variables; this is useful
  when a template is used to generate a non-HTML document.
- `error_on_unknown_variable`: return an error when a variable cannot be found
  in the context.
- `error_on_unknown_partial`: return an error when a partial cannot be found.
- `error_on_invalid_partial`: return an error when a partial cannot be loaded
  or contains errors.
- `return_binary`: return the final document as a binary instead of an iodata
  value.
- `indent`: indent all lines in the template before parsing it by a number of
  space characters equal to the value of this option; this is used for
  standalone partial indentation mandated by the Mustache standard.
- `enable_cache`: lookup templates in the cache before trying to load them.
- `cache_ref`: the reference of the cache process (default:
  `mustache_cache`).

Note that while it may seem surprising that errors are not returned by
default, this is the behaviour recommended by the Mustache standard: unknown
variables, unknown partials and invalid partials are ignored and the empty
string is inserted in the final document. It is strongly recommended to always
enable error reporting.

## Errors
Errors are returned as maps which may contain the following fields:
- `template_name`: the name of the template causing the error.
- `template_full_name`: a more precise identifier of the template, e.g. the
  path of the template for templates loaded from a file.
- `position`: a `{Line, Column}` tuple indicating the location of the error in
  the template source.
- `tag_name`: the name of the Mustache tag causing the error.
- `reason`: a value representing the cause of the error (mandatory).
- `partial_error`: when a partial cannot be loaded or generated, the error
  which was signaled. For example:
  ```erlang
  mustache:render(<<"example">>, {string, <<"Hello {{>a}}!">>},
                  #{name => <<"Alice">>},
                  #{template_sources => #{<<"a">> => {string, <<"{{name}">>}},
                    error_on_invalid_partial => true}).
  ```
  Will return:
  ```erlang
  {error, template_name => <<"example">>,
          template_full_name => <<"example">>,
          position => {1,7},
          tag_name => <<"a">>,
          reason => invalid_partial,
          #{partial_error =>
              #{template_name => <<"a">>,
                template_full_name => <<"a">>,
                position => {1,1},
                reason => truncated_tag}}}
  ```

# Cache
The `mustache_cache` implements `gen_server` and provide a cache for template
documents. When caching is enabled, templates are looked up in the cache
before loading, using the template name as key. If the template is not found
in the cache, it is loaded and inserted in the cache before being returned.

Note that when templates are loaded to be inserted in the cache, the options
used are those passed to the initial call. If a subsequent call use different
options, those will not cause a cache invalidation. This may be surprising,
for example when loading a template with error reporting activated while the
same template was loaded into the cache without error reporting.

It is recommended to either always use the cache or never use it.

Multiple cache processes can be created using `mustache_cache:start_link/1` or
`mustache_cache:start_link/1`. The default cache is registered as
`mustache_cache`.
