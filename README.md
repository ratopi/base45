# base45

A Base45 encoder/decoder in pure Erlang.

Implements the somewhat strange Base45 encoding scheme as defined in
[RFC 9285](https://datatracker.ietf.org/doc/rfc9285/).
It was notably used for encoding the EU Digital COVID Certificate in QR codes.

Source code at https://github.com/ratopi/base45.


## About Base45

Base45 encodes pairs of bytes (16 bit) into 3 characters, and a remaining
single byte into 2 characters. It uses a 45-character subset of US-ASCII
that is compatible with the Alphanumeric mode of QR codes.

The number 45 comes from the QR code standard (ISO/IEC 18004). In
Alphanumeric mode, a QR code supports exactly 45 characters:
the digits `0-9`, the uppercase letters `A-Z`, and 9 special characters.
This mode is more compact than Byte mode, encoding two characters in just
11 bits. Base45 takes advantage of this by mapping binary data exclusively
to these 45 characters, resulting in smaller QR codes than e.g. Base64
(which would require the less efficient Byte mode).

The Base45 alphabet:
`0-9`, `A-Z`, ` ` (space), `$`, `%`, `*`, `+`, `-`, `.`, `/`, `:`


## Import to your project

### Erlang (rebar3)

Use it in your project via rebar dependency:

	{deps, [base45]}.

or for a specific release:

	{deps, [{base45, "3.0.0"}]}.

Or if you like to fetch the source code from github:

    {rebar, {git, "https://github.com/ratopi/base45.git", {tag, "3.0.0"}}}.

### Elixir (Mix)

Add `base45` to your dependencies in `mix.exs`:

    defp deps do
      [
        {:base45, "~> 3.0"}
      ]
    end

Then call the Erlang module directly:

    :base45.encode("Hello!!")
    :base45.decode("%69 VD92EX0")
                   
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


## Error handling

Decoding invalid input will raise an error with one of the following reasons:

Calling decode with an illegal input string (like <<"GGW">>, which leads to 65536),
will throw an `illegal_encoding` exception in a tuple, containing the problematic part of the input:

    {illegal_encoding, <<"GGW">>}

The same applies to character pairs that decode to a value above 255 (e.g. <<"::">> = 2024)
or an input with an invalid length (where length mod 3 == 1):

    {illegal_encoding, <<"::">>}
    {illegal_encoding, <<"X">>}

Calling decode with an illegal input string with illegal base45 characters (that are characters not in
the base45 alphabet), will throw an `illegal_character` exception in a tuple, containing the problematic
part of the input:

	{illegal_character, <<"=">>}


## Feedback and bugs

Feel free to give me any feedback you like via github:
https://github.com/ratopi/base45/issues


## Breaking changes

With version 2.0.0 the typo in error atom "illegale_encoding" was fixed, and is now "illegal_encoding" (w/o "e").

Since version 3.0.0 the decoder additionally rejects:
- Input with invalid length (length mod 3 == 1)
- Character pairs that decode to a value above 255


## License

Apache 2.0 — see [LICENSE](LICENSE) for details.

