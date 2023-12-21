# UnicodeNext

[![Build Status](https://github.com/c42f/UnicodeNext.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/c42f/UnicodeNext.jl/actions/workflows/CI.yml?query=branch%3Amain)

A unicode library which can be upgraded to the latest unicode rules, even on
old Julia versions. Why do this? I want to resolve some JuliaSyntax
compatibility issues mentioned at
https://github.com/JuliaLang/JuliaSyntax.jl/pull/381

The interface here is an amalgamation of Julia's `Unicode` stdlib and the
functions from Base's strings/unicode.jl. The implementation is a Julia port of
the C library [utf8proc](https://github.com/JuliaStrings/utf8proc) which has
traditionally powered Base's unicode functionality.

Ideally I (Claire) hope this library might become the implementation of the
Unicode stdlib and the Base functions from strings/unicode.jl, such as
`islowercase()`, etc. But we'd need to figure out the implications of trying to
make this upgradable without breaking how it's integrated into the Julia
sysimage.
