module UnicodeNext

using Base: ismalformed

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

# Documented in Unicode module
graphemes(s::AbstractString) = GraphemeIterator{typeof(s)}(s)

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

#-------------------------------------------------------------------------------

# Ported version of the libutf8proc C library
include("c_lib.jl")


end
