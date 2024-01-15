module UnicodeNext

using Base: ismalformed

# Ported version of the libutf8proc C library
include("c_lib.jl")

_throw_error(code) = error(errmsg(code))

#-------------------------------------------------------------------------------
# APIs defined in Base strings/unicode.jl

## character column width function ##
"""
    textwidth(c)

Give the number of columns needed to print a character.

# Examples
```jldoctest
julia> textwidth('α')
1

julia> textwidth('⛵')
2
```
"""
function textwidth(c::AbstractChar)
    ismalformed(c) && return 1
    return charwidth(UInt32(c))
end

"""
    textwidth(s::AbstractString)

Give the number of columns needed to print a string.

# Examples
```jldoctest
julia> textwidth("March")
5
```
"""
textwidth(s::AbstractString) = mapreduce(textwidth, +, s; init=0)

"""
    lowercase(c::AbstractChar)

Convert `c` to lowercase.

See also [`uppercase`](@ref), [`titlecase`](@ref).

# Examples
```jldoctest
julia> lowercase('A')
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> lowercase('Ö')
'ö': Unicode U+00F6 (category Ll: Letter, lowercase)
```
"""
function lowercase(c::T) where {T<:AbstractChar}
    isascii(c) ? ('A' <= c <= 'Z' ? c + 0x20 : c) :
                 T(tolower(UInt32(c)))
end

"""
    uppercase(c::AbstractChar)

Convert `c` to uppercase.

See also [`lowercase`](@ref), [`titlecase`](@ref).

# Examples
```jldoctest
julia> uppercase('a')
'A': ASCII/Unicode U+0041 (category Lu: Letter, uppercase)

julia> uppercase('ê')
'Ê': Unicode U+00CA (category Lu: Letter, uppercase)
```
"""
function uppercase(c::T) where {T<:AbstractChar}
    isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) :
                 T(toupper(UInt32(c)))
end

"""
    titlecase(c::AbstractChar)

Convert `c` to titlecase. This may differ from uppercase for digraphs,
compare the example below.

See also [`uppercase`](@ref), [`lowercase`](@ref).

# Examples
```jldoctest
julia> titlecase('a')
'A': ASCII/Unicode U+0041 (category Lu: Letter, uppercase)

julia> titlecase('ǆ')
'ǅ': Unicode U+01C5 (category Lt: Letter, titlecase)

julia> uppercase('ǆ')
'Ǆ': Unicode U+01C4 (category Lu: Letter, uppercase)
```
"""
function titlecase(c::T) where {T<:AbstractChar}
    isascii(c) ? ('a' <= c <= 'z' ? c - 0x20 : c) :
                 T(totitle(UInt32(c)))
end

############################################################################
# returns category code in 0:31 giving Unicode category
function category_code(c::AbstractChar)
    !ismalformed(c) ? category_code(UInt32(c)) : Cint(31)
end

function category_code(x::Integer)
    x ≤ 0x10ffff ? category(UInt32(x)) : Cint(30)
end

# more human-readable representations of the category code
function category_abbrev(c::AbstractChar)
    ismalformed(c) && return "Ma"
    c ≤ '\U10ffff' || return "In"
    category_abbrev(UInt32(c))
end

const _category_abbrevs = ["Cn","Lu","Ll","Lt","Lm","Lo","Mn","Mc","Me","Nd","Nl","No","Pc","Pd","Ps","Pe","Pi","Pf","Po","Sm","Sc","Sk","So","Zs","Zl","Zp","Cc","Cf","Cs","Co"]

# Return the two-letter (nul-terminated) Unicode category string for
# the codepoint (e.g. `"Lu"` or `"Co"`).
function category_abbrev(c::UInt32)
    return _category_abbrevs[category_code(c)+1]
end

# strings corresponding to the category constants
const _category_names = [
    "Other, not assigned",
    "Letter, uppercase",
    "Letter, lowercase",
    "Letter, titlecase",
    "Letter, modifier",
    "Letter, other",
    "Mark, nonspacing",
    "Mark, spacing combining",
    "Mark, enclosing",
    "Number, decimal digit",
    "Number, letter",
    "Number, other",
    "Punctuation, connector",
    "Punctuation, dash",
    "Punctuation, open",
    "Punctuation, close",
    "Punctuation, initial quote",
    "Punctuation, final quote",
    "Punctuation, other",
    "Symbol, math",
    "Symbol, currency",
    "Symbol, modifier",
    "Symbol, other",
    "Separator, space",
    "Separator, line",
    "Separator, paragraph",
    "Other, control",
    "Other, format",
    "Other, surrogate",
    "Other, private use",
    "Invalid, too high",
    "Malformed, bad data",
]

category_string(c) = _category_names[category_code(c)+1]

"""
    Unicode.isassigned(c) -> Bool

Return `true` if the given char or integer is an assigned Unicode code point.

# Examples
```jldoctest
julia> Unicode.isassigned(101)
true

julia> Unicode.isassigned('\\x01')
true
```
"""
isassigned(c) = CATEGORY_CN < category_code(c) <= CATEGORY_CO

## libc character class predicates ##

"""
    islowercase(c::AbstractChar) -> Bool

Tests whether a character is a lowercase letter (according to the Unicode
standard's `Lowercase` derived property).

See also [`isuppercase`](@ref).

# Examples
```jldoctest
julia> islowercase('α')
true

julia> islowercase('Γ')
false

julia> islowercase('❤')
false
```
"""
islowercase(c::AbstractChar) = ismalformed(c) ? false : Bool(islower(UInt32(c)))

# true for Unicode upper and mixed case

"""
    isuppercase(c::AbstractChar) -> Bool

Tests whether a character is an uppercase letter (according to the Unicode
standard's `Uppercase` derived property).

See also [`islowercase`](@ref).

# Examples
```jldoctest
julia> isuppercase('γ')
false

julia> isuppercase('Γ')
true

julia> isuppercase('❤')
false
```
"""
isuppercase(c::AbstractChar) = ismalformed(c) ? false : Bool(isupper(UInt32(c)))

"""
    iscased(c::AbstractChar) -> Bool

Tests whether a character is cased, i.e. is lower-, upper- or title-cased.

See also [`islowercase`](@ref), [`isuppercase`](@ref).
"""
function iscased(c::AbstractChar)
    cat = category_code(c)
    return cat == CATEGORY_LU ||
           cat == CATEGORY_LT ||
           cat == CATEGORY_LL
end


"""
    isdigit(c::AbstractChar) -> Bool

Tests whether a character is a decimal digit (0-9).

See also: [`isletter`](@ref).

# Examples
```jldoctest
julia> isdigit('❤')
false

julia> isdigit('9')
true

julia> isdigit('α')
false
```
"""
isdigit(c::AbstractChar) = (c >= '0') & (c <= '9')

"""
    isletter(c::AbstractChar) -> Bool

Test whether a character is a letter.
A character is classified as a letter if it belongs to the Unicode general
category Letter, i.e. a character whose category code begins with 'L'.

See also: [`isdigit`](@ref).

# Examples
```jldoctest
julia> isletter('❤')
false

julia> isletter('α')
true

julia> isletter('9')
false
```
"""
isletter(c::AbstractChar) = CATEGORY_LU <= category_code(c) <= CATEGORY_LO

"""
    isnumeric(c::AbstractChar) -> Bool

Tests whether a character is numeric.
A character is classified as numeric if it belongs to the Unicode general category Number,
i.e. a character whose category code begins with 'N'.

Note that this broad category includes characters such as ¾ and ௰.
Use [`isdigit`](@ref) to check whether a character is a decimal digit between 0 and 9.

# Examples
```jldoctest
julia> isnumeric('௰')
true

julia> isnumeric('9')
true

julia> isnumeric('α')
false

julia> isnumeric('❤')
false
```
"""
isnumeric(c::AbstractChar) = CATEGORY_ND <= category_code(c) <= CATEGORY_NO

# following C++ only control characters from the Latin-1 subset return true

"""
    iscntrl(c::AbstractChar) -> Bool

Tests whether a character is a control character.
Control characters are the non-printing characters of the Latin-1 subset of Unicode.

# Examples
```jldoctest
julia> iscntrl('\\x01')
true

julia> iscntrl('a')
false
```
"""
iscntrl(c::AbstractChar) = c <= '\x1f' || '\x7f' <= c <= '\u9f'

"""
    ispunct(c::AbstractChar) -> Bool

Tests whether a character belongs to the Unicode general category Punctuation, i.e. a
character whose category code begins with 'P'.

# Examples
```jldoctest
julia> ispunct('α')
false

julia> ispunct('/')
true

julia> ispunct(';')
true
```
"""
ispunct(c::AbstractChar) = CATEGORY_PC <= category_code(c) <= CATEGORY_PO

# \u85 is the Unicode Next Line (NEL) character

"""
    isspace(c::AbstractChar) -> Bool

Tests whether a character is any whitespace character. Includes ASCII characters '\\t',
'\\n', '\\v', '\\f', '\\r', and ' ', Latin-1 character U+0085, and characters in Unicode
category Zs.

# Examples
```jldoctest
julia> isspace('\\n')
true

julia> isspace('\\r')
true

julia> isspace(' ')
true

julia> isspace('\\x20')
true
```
"""
@inline isspace(c::AbstractChar) =
    c == ' ' || '\t' <= c <= '\r' || c == '\u85' ||
    '\ua0' <= c && category_code(c) == CATEGORY_ZS

"""
    isprint(c::AbstractChar) -> Bool

Tests whether a character is printable, including spaces, but not a control character.

# Examples
```jldoctest
julia> isprint('\\x01')
false

julia> isprint('A')
true
```
"""
isprint(c::AbstractChar) = CATEGORY_LU <= category_code(c) <= CATEGORY_ZS

# true in principal if a printer would use ink

"""
    isxdigit(c::AbstractChar) -> Bool

Test whether a character is a valid hexadecimal digit. Note that this does not
include `x` (as in the standard `0x` prefix).

# Examples
```jldoctest
julia> isxdigit('a')
true

julia> isxdigit('x')
false
```
"""
isxdigit(c::AbstractChar) = '0'<=c<='9' || 'a'<=c<='f' || 'A'<=c<='F'

## uppercase, lowercase, and titlecase transformations ##

"""
    uppercase(s::AbstractString)

Return `s` with all characters converted to uppercase.

See also [`lowercase`](@ref), [`titlecase`](@ref), [`uppercasefirst`](@ref).

# Examples
```jldoctest
julia> uppercase("Julia")
"JULIA"
```
"""
uppercase(s::AbstractString) = map(uppercase, s)

"""
    lowercase(s::AbstractString)

Return `s` with all characters converted to lowercase.

See also [`uppercase`](@ref), [`titlecase`](@ref), [`lowercasefirst`](@ref).

# Examples
```jldoctest
julia> lowercase("STRINGS AND THINGS")
"strings and things"
```
"""
lowercase(s::AbstractString) = map(lowercase, s)

"""
    titlecase(s::AbstractString; [wordsep::Function], strict::Bool=true) -> String

Capitalize the first character of each word in `s`;
if `strict` is true, every other character is
converted to lowercase, otherwise they are left unchanged.
By default, all non-letters beginning a new grapheme are considered as word separators;
a predicate can be passed as the `wordsep` keyword to determine
which characters should be considered as word separators.
See also [`uppercasefirst`](@ref) to capitalize only the first
character in `s`.

See also [`uppercase`](@ref), [`lowercase`](@ref), [`uppercasefirst`](@ref).

# Examples
```jldoctest
julia> titlecase("the JULIA programming language")
"The Julia Programming Language"

julia> titlecase("ISS - international space station", strict=false)
"ISS - International Space Station"

julia> titlecase("a-a b-b", wordsep = c->c==' ')
"A-a B-b"
```
"""
function titlecase(s::AbstractString; wordsep::Function = !isletter, strict::Bool=true)
    startword = true
    state = GraphemeState()
    c0 = eltype(s)(0x00000000)
    b = IOBuffer()
    for c in s
        # Note: It would be better to have a word iterator following UAX#29,
        # similar to our grapheme iterator, but utf8proc does not yet have
        # this information.  At the very least we shouldn't break inside graphemes.
        state, isbreak = isgraphemebreak(state, c0, c)
        if isbreak && wordsep(c)
            print(b, c)
            startword = true
        else
            print(b, startword ? titlecase(c) : strict ? lowercase(c) : c)
            startword = false
        end
        c0 = c
    end
    return String(take!(b))
end

"""
    uppercasefirst(s::AbstractString) -> String

Return `s` with the first character converted to uppercase (technically "title
case" for Unicode). See also [`titlecase`](@ref) to capitalize the first
character of every word in `s`.

See also [`lowercasefirst`](@ref), [`uppercase`](@ref), [`lowercase`](@ref),
[`titlecase`](@ref).

# Examples
```jldoctest
julia> uppercasefirst("python")
"Python"
```
"""
function uppercasefirst(s::AbstractString)
    isempty(s) && return ""
    c = s[1]
    c′ = titlecase(c)
    c == c′ ? convert(String, s) :
    string(c′, SubString(s, nextind(s, 1)))
end

"""
    lowercasefirst(s::AbstractString)

Return `s` with the first character converted to lowercase.

See also [`uppercasefirst`](@ref), [`uppercase`](@ref), [`lowercase`](@ref),
[`titlecase`](@ref).

# Examples
```jldoctest
julia> lowercasefirst("Julia")
"julia"
```
"""
function lowercasefirst(s::AbstractString)
    isempty(s) && return ""
    c = s[1]
    c′ = lowercase(c)
    c == c′ ? convert(String, s) :
    string(c′, SubString(s, nextind(s, 1)))
end

############################################################################
# iterators for grapheme segmentation

struct GraphemeState
    boundclass::UInt8
    indic_conjunct_break::UInt8
end

GraphemeState() = GraphemeState(0, 0)

# Stateful grapheme break required by Unicode-9 rules: the string
# must be processed in sequence, with state initialized to Ref{Int32}(0).
# Requires utf8proc v2.0 or later.
function isgraphemebreak(state::GraphemeState, c1::AbstractChar, c2::AbstractChar)
    if Base.ismalformed(c1) || Base.ismalformed(c2)
        return (true, GraphemeState())
    end
    u1 = UInt32(c1)
    u2 = UInt32(c2)
    packedstate = state.boundclass | (state.indic_conjunct_break << 8)
    p1 = get_property(u1)
    p2 = get_property(u2)
    break_permitted, packedstate =
        _grapheme_break_extended(p1.boundclass, p2.boundclass,
                                 p1.indic_conjunct_break, p2.indic_conjunct_break,
                                 packedstate)
    state = GraphemeState(packedstate & 0xff, packedstate >> 8)
    return (state, break_permitted)
end

struct GraphemeIterator{S<:AbstractString}
    s::S # original string (for generation of SubStrings)
end

Base.eltype(::Type{GraphemeIterator{S}}) where {S} = SubString{S}
Base.eltype(::Type{GraphemeIterator{SubString{S}}}) where {S} = SubString{S}

function Base.length(g::GraphemeIterator{S}) where {S}
    c0 = eltype(S)(0x00000000)
    n = 0
    state = GraphemeState()
    for c in g.s
        state, isbreak = isgraphemebreak(state, c0, c)
        n += isbreak
        c0 = c
    end
    return n
end

function Base.iterate(g::GraphemeIterator, i_=(GraphemeState(),firstindex(g.s)))
    s = g.s
    state, i = i_
    j = i
    y = iterate(s, i)
    y === nothing && return nothing
    c0, k = y
    while k <= ncodeunits(s) # loop until next grapheme is s[i:j]
        c, ℓ = iterate(s, k)::NTuple{2,Any}
        state, isbreak = isgraphemebreak(state, c0, c)
        isbreak && break
        j = k
        k = ℓ
        c0 = c
    end
    return (SubString(s, i, j), (state, k))
end

Base.:(==)(g1::GraphemeIterator, g2::GraphemeIterator) = g1.s == g2.s
Base.hash(g::GraphemeIterator, h::UInt) = hash(g.s, h)
Base.isless(g1::GraphemeIterator, g2::GraphemeIterator) = isless(g1.s, g2.s)

Base.show(io::IO, g::GraphemeIterator{S}) where {S} = print(io, "length-$(length(g)) GraphemeIterator{$S} for \"$(g.s)\"")

"""
    graphemes(s::AbstractString) -> GraphemeIterator

Return an iterator over substrings of `s` that correspond to the extended graphemes in the
string, as defined by Unicode UAX #29. (Roughly, these are what users would perceive as
single characters, even though they may contain more than one codepoint; for example a
letter combined with an accent mark is a single grapheme.)
"""
graphemes(s::AbstractString) = GraphemeIterator{typeof(s)}(s)

"""
    graphemes(s::AbstractString, m:n) -> SubString

Returns a [`SubString`](@ref) of `s` consisting of the `m`-th
through `n`-th graphemes of the string `s`, where the second
argument `m:n` is an integer-valued [`AbstractUnitRange`](@ref).

Loosely speaking, this corresponds to the `m:n`-th user-perceived
"characters" in the string.  For example:

```jldoctest
julia> s = graphemes("exposé", 3:6)
"posé"

julia> collect(s)
5-element Vector{Char}:
 'p': ASCII/Unicode U+0070 (category Ll: Letter, lowercase)
 'o': ASCII/Unicode U+006F (category Ll: Letter, lowercase)
 's': ASCII/Unicode U+0073 (category Ll: Letter, lowercase)
 'e': ASCII/Unicode U+0065 (category Ll: Letter, lowercase)
 '́': Unicode U+0301 (category Mn: Mark, nonspacing)
```
This consists of the 3rd to *7th* codepoints ([`Char`](@ref)s) in `"exposé"`,
because the grapheme `"é"` is actually *two* Unicode codepoints
(an `'e'` followed by an acute-accent combining character U+0301).

Because finding grapheme boundaries requires iteration over the
string contents, the `graphemes(s, m:n)` function requires time
proportional to the length of the string (number of codepoints)
before the end of the substring.

!!! compat "Julia 1.9"
    The `m:n` argument of `graphemes` requires Julia 1.9.
"""
function graphemes(s::AbstractString, r::AbstractUnitRange{<:Integer})
    m, n = Int(first(r)), Int(last(r))
    m > 0 || throw(ArgumentError("starting index $m is not ≥ 1"))
    n < m && return @view s[1:0]
    c0 = eltype(s)(0x00000000)
    state = GraphemeState()
    count = 0
    i, iprev, ilast = 1, 1, lastindex(s)
    # find the start of the m-th grapheme
    while i ≤ ilast && count < m
        @inbounds c = s[i]
        state, isbreak = isgraphemebreak(state, c0, c)
        count += isbreak
        c0 = c
        i, iprev = nextind(s, i), i
    end
    start = iprev
    count < m && throw(BoundsError(s, i))
    # find the end of the n-th grapheme
    while i ≤ ilast
        @inbounds c = s[i]
        state, isbreak = isgraphemebreak(state, c0, c)
        count += isbreak
        count > n && break
        c0 = c
        i, iprev = nextind(s, i), i
    end
    count < n && throw(BoundsError(s, i))
    return @view s[start:iprev]
end

function _decompose_char!(codepoint::Union{Integer,Char},
        dest::Base.RefValue{NTuple{MAX_DECOMPOSE_CODEPOINTS,UInt32}}, options::Integer)
    dest_buf = Ptr{UInt32}(Base.pointer_from_objref(dest))
    last_boundclass = Ref{Int32}(0)
    ret = GC.@preserve dest decompose_char(UInt32(codepoint), dest_buf,
                                        MAX_DECOMPOSE_CODEPOINTS, options, last_boundclass)
    ret < 0 && _throw_error(ret)
    return ret
end

"""
    isequal_normalized(s1::AbstractString, s2::AbstractString; casefold=false, stripmark=false, chartransform=identity)

Return whether `s1` and `s2` are canonically equivalent Unicode strings.   If `casefold=true`,
ignores case (performs Unicode case-folding); if `stripmark=true`, strips diacritical marks
and other combining characters.

As with [`Unicode.normalize`](@ref), you can also pass an arbitrary
function via the `chartransform` keyword (mapping `Integer` codepoints to codepoints)
to perform custom normalizations, such as [`Unicode.julia_chartransform`](@ref).

# Examples

For example, the string `"noël"` can be constructed in two canonically equivalent ways
in Unicode, depending on whether `"ë"` is formed from a single codepoint U+00EB or
from the ASCII character `'e'` followed by the U+0308 combining-diaeresis character.

```jldoctest
julia> s1 = "no\u00EBl"
"noël"

julia> s2 = "noe\u0308l"
"noël"

julia> s1 == s2
false

julia> isequal_normalized(s1, s2)
true

julia> isequal_normalized(s1, "noel", stripmark=true)
true

julia> isequal_normalized(s1, "NOËL", casefold=true)
true
```
"""
function isequal_normalized(s1::AbstractString, s2::AbstractString; casefold::Bool=false, stripmark::Bool=false, chartransform=identity)
    function decompose_next_char!(c, state, d, options, s)
        n = _decompose_char!(c, d, options)
        n < 0 && _throw_error(n)
        return 1, n, iterate(s, state)
    end
    options = DECOMPOSE
    casefold && (options |= CASEFOLD)
    stripmark && (options |= STRIPMARK)
    i1,i2 = iterate(s1),iterate(s2)
    # codepoint buffers
    d1 = Ref{NTuple{MAX_DECOMPOSE_CODEPOINTS,UInt32}}()
    d2 = Ref{NTuple{MAX_DECOMPOSE_CODEPOINTS,UInt32}}()
    n1 = n2 = 0 # lengths of codepoint buffers
    j1 = j2 = 1 # indices in d1, d2
    while true
        if j1 > n1
            i1 === nothing && return i2 === nothing && j2 > n2
            j1, n1, i1 = decompose_next_char!(chartransform(UInt32(i1[1])), i1[2], d1, options, s1)
        end
        if j2 > n2
            i2 === nothing && return false
            j2, n2, i2 = decompose_next_char!(chartransform(UInt32(i2[1])), i2[2], d2, options, s2)
        end
        d1[][j1] == d2[][j2] || return false
        j1 += 1; j2 += 1
    end
end



#-------------------------------------------------------------------------------

end
