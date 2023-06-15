# base45

An Base45-encoder/decoder in pure Erlang.
                    
Source code at https://github.com/ratopi/base45.

Implementing encoding as defined in RFC 9285 (https://datatracker.ietf.org/doc/rfc9285/).


## Import to your project

Use it in your project via rebar dependency:

	{deps, [base45]}.

or for a specific release:

	{deps, [{base45, "2.0.0"]}.

Or if you like to fetch the source code from github:

    {rebar, {git, "https://github.com/ratopi/base45.git", {tag, "2.0.0"}}}.
                   
See https://hex.pm/packages/base45 for more info about the hex package.

Overview of current releases are on the hex-page or at
https://github.com/ratopi/base45/releases.
                  

## Usage

Usage is just straight forward.
Currently only binaries are supported.

Encoding:

    base45:encode(<<1,2,3>>).

gives 

    <<"X5030">>

Decoding:

    base45:decode(<<"X5030">>).

gives

    <<1,2,3>>

Calling decode with an illegal input string (like <<"GGW">>, which leads to 65536),
will throw an `illegal_encoding` exception in a tuple, containing the problematic part of the input:

    {illegal_encoding, <<"GGW">>}

Calling decode with an illegal input string with illegal base45 characters (that are characters not in
the base45 alphabet), will throw an `illegal_character` exception in a tuple, containing the problematic
part of the input:

	{illegal_character, <<"=">>}


## Feedback and bugs

Feel free to give me any feedback you like via github:
https://github.com/ratopi/base45/issues


## Breaking changes

With version 2.0.0 the typo in error atom "illegale_encoding" was fixed, and is now "illegal_encoding" (w/o "e").

