/*
Module: parser

Convert a list of tokens into a command_line.
*/

#[link(name = "parser",
       vers = "0.1")];

use std;
use tokenizer;
import either::either;
import either::right;
import either::left;
import tokenizer::token;
import tokenizer::token_to_string;

export parse;

enum output_sink {
    stdout,
    stderr,
    outfile(str),
}

enum input_source {
    stdin,
    infile(str),
}

type command = {args: [str],
                input: input_source,
                output: output_sink,
                error: output_sink };

enum command_line {
    singleton(command),
    pipeline([command_line]),
    sequence([command_line]),
    background(@command_line),
    and([command_line]),
    or([command_line]),
}

enum parse_result {
    parsed(command_line),
    continuation_required,
    error(str),
}

/*
Function: parse

Parse a sequence of tokens into a command line.

Parameters:

tokens - A vector of tokens from a command line.

Returns:

A parse_result. If continuation_required is returned, the caller must
harvest another commandline from the user, and call parse again with
the new tokens concatenated to the present tokens.
*/
fn parse(tokens: [token]) -> parse_result {
    if vec::is_empty(tokens) {
        ret parsed(sequence([]));
    } else if vec::last(tokens) == tokenizer::continuation {
        ret continuation_required;
    }
    let idx = 0u;
    ret parse_tokens(tokens, 0u, idx);
}

fn make_command(tokens: [token]) -> either<command, str> {
    assert vec::is_not_empty(tokens);
    let args: [str] = [];
    let i = stdin;
    let o = stdout;
    let e = stderr;
    for t in tokens {
        alt t {
          tokenizer::string(s) { args += [s]; }
          tokenizer::redirect_output(s) {
            if (o != stdout) { ret right("Multiple output redirects."); }
            o = outfile(s);
          }
          tokenizer::redirect_error(s) {
            if (e != stderr) { ret right("Multiple error redirects."); }
            e = outfile(s);
          }
          tokenizer::redirect_error_to_output {
            if (e != stderr) { ret right("Multiple error redirects."); }
            e = o;
          }
          tokenizer::redirect_input(s) {
            if (i != stdin) { ret right("Multiple input redirects."); }
            i = infile(s);
          }
          _ { ret right("Unexpected token: " + token_to_string(t)); }
        }
    }
    ret left({args: args, input: i, output: o, error: e});
}

enum part_parse {
    cmd(command),
    subshell(command_line),
    sep(token),
}

fn part_to_cl(p: part_parse) -> command_line {
    ret alt p {
      cmd(c) { singleton(c) }
      subshell(cl) { cl }
      sep(_) { fail("part_to_cl doesn't convert separators."); }
    };
}

fn append_to_cl(&cl: command_line, p: part_parse) {
    cl = alt cl {
      singleton(_) { fail("cl may not be singleton."); }
      pipeline(args) { pipeline(args + [part_to_cl(p)]) }
      sequence(args) { sequence(args + [part_to_cl(p)]) }
      background(_) { sequence([cl, part_to_cl(p)]) }
      and(args) { and(args + [part_to_cl(p)]) }
      or(args) { or(args + [part_to_cl(p)]) }
    };
}

fn finish_parse(parts: [part_parse]) -> parse_result {
    assert vec::is_not_empty(parts);
    let cur_cl = alt parts[0u] {
      sep(_)  { ret error("No initial command."); }
      _ { part_to_cl(parts[0u]) }
    };
    if vec::len(parts) == 1u {
        ret parsed(cur_cl);
    }
    let idx = 1u;
    let cmd_required = false;
    let cmd_allowed = false;
    while idx < vec::len(parts) {
        alt parts[idx] {
          cmd(_)
          | subshell(_) {
            if !cmd_allowed {
                ret error("Found a command where a separator was expected.");
            }
            cmd_required = false;
            cmd_allowed = false;
            alt cur_cl {
              _ { append_to_cl(cur_cl, parts[idx]); }
            }
          }
          sep(t) {
            if cmd_required {
                ret error("Found a separator where a command was expected.");
            }
            cmd_required = true;
            cmd_allowed = true;
            cur_cl = alt t {
              tokenizer::pipe {
                alt cur_cl {
                  pipeline(_) { cur_cl }
                  _ { pipeline([cur_cl]) }
                }
              }
              tokenizer::and {
                alt cur_cl {
                  and(_) { cur_cl }
                  _ { and([cur_cl]) }
                }
              }
              tokenizer::or {
                alt cur_cl {
                  or(_) { cur_cl }
                  _ { or([cur_cl]) }
                }
              }
              tokenizer::background {
                cmd_required = false;
                background(@cur_cl)
              }
              tokenizer::sequence {
                alt cur_cl {
                  sequence(_) { cur_cl }
                  _ { sequence([cur_cl]) }
                }
              }
              _ {
                fail("Unexpected partial parse.");
              }
            }
          }
        }
        idx += 1u;
    }
    ret if cmd_required {
        error("Missing command at end of line.")
    } else {
        parsed(cur_cl)
    };
}

fn parse_tokens(tokens: [token], level: uint, &idx: uint) -> parse_result {
    let parts: [part_parse] = [];
    let cur: [token] = [];

    #macro([#make_command[ts, ps],
            if vec::is_not_empty(ts) {
                alt make_command(ts) {
                  left(c) { ps += [cmd(c)]; ts = []; }
                  right(e) { ret error(e); }
                }
            }]);

    while idx < vec::len(tokens) {
        let t = tokens[idx];
        alt t {
          tokenizer::error(e) { ret error(e); }
          tokenizer::pipe
          | tokenizer::and
          | tokenizer::or
          | tokenizer::background
          | tokenizer::sequence {
            #make_command[cur, parts];
            parts += [sep(t)];
          }
          tokenizer::open_subshell {
            #make_command[cur, parts];
            idx += 1u;
            alt parse_tokens(tokens, level + 1u, idx) {
              parsed(cl) { parts += [subshell(cl)]; }
              error(e) { ret error(e); }
              continuation_required { fail("Inconceivable!"); }
            }
          }
          tokenizer::close_subshell {
            if level == 0u {
                ret error("Unexpected ')'.");
            }
            #make_command[cur, parts];
            ret finish_parse(parts);
          }
          tokenizer::continuation {  /* ignore me! */ }
          _ { cur += [t]; }
        }
        idx += 1u;
    }
    if level > 0u {
        ret error("Expected ')'");
    }
    #make_command[cur, parts];
    ret finish_parse(parts);
}

#[test]
fn test_make_command() {
    assert make_command([tokenizer::string("foo"),
                         tokenizer::string("bar"),
                         tokenizer::redirect_output("baz"),
                         tokenizer::redirect_error_to_output])
        == left({args: ["foo", "bar"],
                 input: stdin,
                 output: outfile("baz"),
                 error: outfile("baz")});
    assert make_command([tokenizer::string("foo"),
                         tokenizer::string("bar"),
                         tokenizer::redirect_error_to_output,
                         tokenizer::redirect_output("baz")])
        == left({args: ["foo", "bar"],
                 input: stdin,
                 output: outfile("baz"),
                 error: stdout});
    assert make_command([tokenizer::string("foo"),
                         tokenizer::string("bar"),
                         tokenizer::redirect_input("hootenanny"),
                         tokenizer::redirect_output("baz")])
        == left({args: ["foo", "bar"],
                 input: infile("hootenanny"),
                 output: outfile("baz"),
                 error: stderr});
    assert make_command([tokenizer::string("foo"),
                         tokenizer::string("bar"),
                         tokenizer::redirect_error_to_output,
                         tokenizer::redirect_error("/dev/null"),
                         tokenizer::redirect_output("baz")])
        == right("Multiple error redirects.");
    alt make_command([tokenizer::string("foo"),
                      tokenizer::string("bar"),
                      tokenizer::background]) {
      left(_) { assert false; }
      right(_) { assert true; }
    }
}

#[test]
fn simple_cmdline() {
    assert parse(tokenizer::tokenize("  hi there"))
        == parsed(singleton({args: ["hi", "there"],
                             input: stdin,
                             output: stdout,
                             error: stderr}));
}

#[test]
fn complex_pipeline() {
    assert parse(tokenizer::tokenize("(cat abc d\"e f\\\"\"g; echo 'hello\\') |"
                                     + " grep -i he >matches &"))
        == parsed(background(@pipeline(
            [sequence([singleton({args: ["cat", "abc", "de f\"g"],
                                  input: stdin,
                                  output: stdout,
                                  error: stderr}),
                       singleton({args: ["echo", "hello\\"],
                                  input: stdin,
                                  output: stdout,
                                  error: stderr})]),
             singleton({args: ["grep", "-i", "he"],
                        input: stdin,
                        output: outfile("matches"),
                        error: stderr})])));
}

#[test]
fn test_continuation() {
    assert parse(tokenizer::tokenize("foo && bar && \\"))
        == continuation_required;
}

#[test]
fn test_binary_operators() {
    alt parse(tokenizer::tokenize("foo && | bar")) {
      error(_) { assert true; }
      _ { assert false; }
    }

    assert parse(tokenizer::tokenize("foo && bar && baz"))
        == parsed(and([singleton({args: ["foo"],
                                  input: stdin,
                                  output: stdout,
                                  error: stderr}),
                       singleton({args: ["bar"],
                                  input: stdin,
                                  output: stdout,
                                  error: stderr}),
                       singleton({args: ["baz"],
                                  input: stdin,
                                  output: stdout,
                                  error: stderr})]));

    alt parse(tokenizer::tokenize("foo && bar &&")) {
      error(_) { assert true; }
      _ { assert false; }
    }
}
