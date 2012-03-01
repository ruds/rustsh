/*
Module: tokenize

Command-line tokenizing
*/
use std;

export token;
export tokenize;

enum token {
    string(str),
    pipe,  // |
    redirect_output(str),  // > file
    redirect_input(str),  // < file
    and,  // &&
    or,  // ||
    background,  // &
    sequence,  // ;
    open_subshell,  // (
    close_subshell,  // )
    continuation,  // \
    error(str),
}

type consumption = {
    t: token,
    offset: uint,
};

fn make_string_consumption(c: [char], offset: uint, end: uint) -> consumption {
    ret {t: string(str::from_chars(vec::slice(c, offset, end))),
         offset: end};
}

fn consume_whitespace(c: [char], offset: uint) -> consumption {
    let end = offset;
    while (end < vec::len(c) 
           && str::is_whitespace(str::from_char(c[end]))) {
        end += 1u;
    }
    ret make_string_consumption(c, offset, end);
}

fn consume_or(c: [char], offset: uint) -> consumption {
    if vec::len(c) < offset + 2u || c[offset] != '|' || c[offset + 1u] != '|' {
        fail(#fmt("Tried to consume || at %u of '%s'.",
                  offset, str::from_chars(c)));
    }
    ret {t: or, offset: offset + 2u};
}

fn consume_and(c: [char], offset: uint) -> consumption {
    if vec::len(c) < offset + 2u || c[offset] != '&' || c[offset + 1u] != '&' {
        fail(#fmt("Tried to consume && at %u of '%s'.",
                  offset, str::from_chars(c)));
    }
    ret {t: and, offset: offset + 2u};
}

fn consume_pipe(c: [char], offset: uint) -> consumption {
    assert c[offset] == '|';
    ret {t: pipe, offset: offset + 1u};
}

fn consume_background(c: [char], offset: uint) -> consumption {
    assert c[offset] == '&';
    ret {t: background, offset: offset + 1u};
}

fn consume_redirect_output(c: [char], offset: uint) -> consumption {
    assert c[offset] == '>';
    let {t:_, offset: ws_offset} = consume_whitespace(c, offset + 1u);
    ret alt consume_string(c, ws_offset) {
      {t: string(file_name), offset: end} {
        if str::len(file_name) > 0u {
            {t: redirect_output(file_name), offset: end}
        } else {
            {t: error("No output file specified."), offset: vec::len(c) }
        }
      }
      _ {
        {t: error("Could not parse file name for output redirection."),
         offset: vec::len(c) }
      }
    };
}
        
fn consume_redirect_input(c: [char], offset: uint) -> consumption {
    assert c[offset] == '<';
    let {t:_, offset: ws_offset} = consume_whitespace(c, offset + 1u);
    ret alt consume_string(c, ws_offset) {
      {t: string(file_name), offset: end} {
        if str::len(file_name) > 0u {
            {t: redirect_input(file_name), offset: end}
        } else {
            {t: error("No input file specified."), offset: vec::len(c) }
        }
      }
      _ {
        {t: error("Could not parse file name for input redirection."),
         offset: vec::len(c)}
      }
    };
}

fn consume_sequence(c: [char], offset: uint) -> consumption {
    assert c[offset] == ';';
    ret {t: sequence, offset: offset + 1u};
}

fn consume_open_subshell(c: [char], offset: uint) -> consumption {
    assert c[offset] == '(';
    ret {t: open_subshell, offset: offset + 1u};
}

fn consume_close_subshell(c: [char], offset: uint) -> consumption {
    assert c[offset] == ')';
    ret {t: close_subshell, offset: offset + 1u};
}

fn consume_singleq(c: [char], offset: uint) -> consumption {
    assert c[offset] == '\'';
    let end = offset + 1u;
    while end < vec::len(c) && c[end] != '\'' {
        end += 1u;
    }
    ret if end == vec::len(c) {
        {t: error("Missing '."), offset: end}
    } else {
        let t = make_string_consumption(c, offset + 1u, end);
        {t: t.t, offset: t.offset + 1u}
    };
}

fn consume_doubleq(c: [char], offset: uint) -> consumption {
    #debug("consume_doubleq called: '%s', %u", str::from_chars(c), offset);
    assert c[offset] == '"';
    let s: str = "";
    let end = offset + 1u;
    while end < vec::len(c) && c[end] != '"' {
        if c[end] == '\\' && end + 1u != vec::len(c) {
            alt c[end + 1u] {
              '"' {
                str::push_char(s, '"');
                end += 2u;
              }
              '\\' {
                str::push_char(s, '\\');
                end += 2u;
              }
              _ {
                str::push_char(s, '\\');
                str::push_char(s, c[end + 1u]);
                end += 2u;
              }
            }
        } else {
            str::push_char(s, c[end]);
            end += 1u;
        }
    }
    ret if end == vec::len(c) {
        {t: error("Missing \"."), offset: end}
    } else {
        {t: string(s), offset: end + 1u}
    };
}

fn consume_string(c: [char], offset: uint) -> consumption {
    #debug("consume_string called: '%s', %u", str::from_chars(c), offset);
    let s: str = "";
    let end = offset;
    while end < vec::len(c) {
        if str::is_whitespace(str::from_char(c[end])) {
            break;
        } else {
            alt c[end] {
              '<' | '>' | ';' | '&' | '|' | '(' | ')' {
                break;
              }
              '\\' {
                if end + 1u < vec::len(c) {
                    str::push_char(s, '\\');
                    end += 1u;
                } else {
                    break;
                }
              }
              '"' {
                let r = consume_doubleq(c, end);
                alt r {
                  {t: string(qs), offset: s_offset}  {
                    s += qs;
                    end = s_offset;
                  }
                  {t: error(_), offset: _} {
                    ret r;
                  }
                  _ {
                    fail("consume_doubleq returned an unexpected type.");
                  }
                }
              }
              '\'' {
                let r = consume_singleq(c, end);
                alt r {
                  {t: string(qs), offset: s_offset}  {
                    s += qs;
                    end = s_offset;
                  }
                  {t: error(_), offset: _} {
                    ret r;
                  }
                  _ {
                    fail("consume_doubleq returned an unexpected type.");
                  }
                }
              }
              _ {
                str::push_char(s, c[end]);
                end += 1u;
              }
            }
        }
    }
    ret {t: string(s), offset: end};
}

fn consume_token(c: [char], offset: uint) -> consumption {
    #debug("consume_token called: '%s', %u", str::from_chars(c), offset);
    let t: consumption =
        alt c[offset] {
          '|' {
            if offset + 1u < vec::len(c) && c[offset + 1u] == '|' {
                consume_or(c, offset)
            } else {
                consume_pipe(c, offset)
            }
          }
          '>' {
            consume_redirect_output(c, offset)
          }
          '<' {
            consume_redirect_input(c, offset)
          }
          '&' {
            if offset + 1u < vec::len(c) && c[offset + 1u] == '&' {
                consume_and(c, offset)
            } else {
                consume_background(c, offset)
            }
          }
          ';' {
            consume_sequence(c, offset)
          }
          '(' {
            consume_open_subshell(c, offset)
          }
          ')' {
            consume_close_subshell(c, offset)
          }
          '\\' {
            if offset + 1u < vec::len(c) {
                consume_string(c, offset)
            } else {
                {t: continuation, offset: offset + 1u}
            }
          }
          _ {
            consume_string(c, offset)
          }
        };
    let {t:_, offset: end} = consume_whitespace(c, t.offset);
    ret {t: t.t, offset: end};
}

/*
Function: tokenize

Tokenize a command line

Parameters:

cmd_line - the command line that the user typed; should not be terminated by \n

Returns:

A vector of tokens
*/
fn tokenize(cmd_line: str) -> [token] {
    let tokens: [token] = [];
    let c = str::chars(cmd_line);

    let {t:_, offset} = consume_whitespace(c, 0u);
    while offset != vec::len(c) {
        let t = consume_token(c, offset);
        offset = t.offset;
        tokens += [t.t];
    }
    ret tokens;
}

#[test]
fn simple_cmdline() {
    let ts = tokenize("  hi there");
    log(info, ts);
    assert ts == [string("hi"), string("there")];
}

#[test]
fn all_whitespace() {
    assert tokenize("") == [];
    assert tokenize("   \t") == [];
}

#[test]
fn complex_pipeline() {
    let ts = tokenize("(cat abc d\"e f\\\"\"g; echo 'hello\\') |"
                      + "grep -i he >matches &");
    log(info, ts);
    assert ts == [open_subshell, string("cat"), string("abc"),
                  string("de f\"g"), sequence, string("echo"),
                  string("hello\\"), close_subshell, pipe, string("grep"),
                  string("-i"), string("he"), redirect_output("matches"),
                  background];
}

#[test]
fn test_redirection() {
    assert tokenize("wc -l < file.txt") == [string("wc"), string("-l"),
                                            redirect_input("file.txt")];
    assert tokenize("wc<in>out") == [string("wc"), redirect_input("in"),
                                     redirect_output("out")];
    assert tokenize("wc < >&") ==
        [string("wc"), error("No input file specified.")];
}

#[test]
fn test_continuation() {
    let ts = tokenize("foo && bar &&\\");
    log(info, ts);
    assert ts == [string("foo"), and, string("bar"), and, continuation];
}

#[test]
fn unterminated_string() {
    let ts = tokenize("foo \"bar baz");
    log(info, ts);
    assert ts == [string("foo"), error("Missing \".")];
}
