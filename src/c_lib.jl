# Copyright (c) 2014-2021 Steven G. Johnson, Jiahao Chen, Peter Colberg, Tony Kelman, Scott P. Jones, Claire Foster and other contributors.
# Copyright (c) 2009 Public Software Group e. V., Berlin, Germany
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.

# Option flags used by several functions in the library.
# The given UTF-8 input is NULL terminated.
const NULLTERM  = (1<<0)
# Unicode Versioning Stability has to be respected.
const STABLE    = (1<<1)
# Compatibility decomposition (i.e. formatting information is lost).
const COMPAT    = (1<<2)
# Return a result with decomposed characters.
const COMPOSE   = (1<<3)
# Return a result with decomposed characters.
const DECOMPOSE = (1<<4)
# Strip "default ignorable characters" such as SOFT-HYPHEN or ZERO-WIDTH-SPACE.
const IGNORE    = (1<<5)
# Return an error, if the input contains unassigned codepoints.
const REJECTNA  = (1<<6)
#
# Indicating that NLF-sequences (LF, CRLF, CR, NEL) are representing a
# line break, and should be converted to the codepoint for line
# separation (LS).
#
const NLF2LS    = (1<<7)
#
# Indicating that NLF-sequences are representing a paragraph break, and
# should be converted to the codepoint for paragraph separation
# (PS).
#
const NLF2PS    = (1<<8)
# Indicating that the meaning of NLF-sequences is unknown.
const NLF2LF    = (NLF2LS | NLF2PS)
# Strips and/or convers control characters.
#
# NLF-sequences are transformed into space, except if one of the
# NLF2LS/PS/LF options is given. HorizontalTab (HT) and FormFeed (FF)
# are treated as a NLF-sequence in this case.  All other control
# characters are simply removed.
#
const STRIPCC   = (1<<9)
#
# Performs unicode case folding, to be able to do a case-insensitive
# string comparison.
#
const CASEFOLD  = (1<<10)
#
# Inserts 0xFF bytes at the beginning of each sequence which is
# representing a single grapheme cluster (see UAX#29).
#
const CHARBOUND = (1<<11)
# Lumps certain characters together.
#
# E.g. HYPHEN U+2010 and MINUS U+2212 to ASCII "-". See lump.md for details.
#
# If NLF2LF is set, this includes a transformation of paragraph and
# line separators to ASCII line-feed (LF).
#
const LUMP      = (1<<12)
# Strips all character markings.
#
# This includes non-spacing, spacing and enclosing (i.e. accents).
# @note This option works only with @ref COMPOSE or
#       @ref DECOMPOSE
#
const STRIPMARK = (1<<13)
#
# Strip unassigned codepoints.
#
const STRIPNA    = (1<<14)

# Error codes being returned by almost all functions.
#
# Memory could not be allocated.
const ERROR_NOMEM = -1
# The given string is too long to be processed.
const ERROR_OVERFLOW = -2
# The given string is not a legal UTF-8 string.
const ERROR_INVALIDUTF8 = -3
# The @ref REJECTNA flag was set and an unassigned codepoint was found.
const ERROR_NOTASSIGNED = -4
# Invalid options have been used.
const ERROR_INVALIDOPTS = -5

#-------------------------------------------------------------------------------
# Internal constants
const HANGUL_SBASE  = 0xAC00
const HANGUL_LBASE  = 0x1100
const HANGUL_VBASE  = 0x1161
const HANGUL_TBASE  = 0x11A7
const HANGUL_LCOUNT = 19
const HANGUL_VCOUNT = 21
const HANGUL_TCOUNT = 28
const HANGUL_NCOUNT = 588
const HANGUL_SCOUNT = 11172
# END is exclusive
const HANGUL_L_START  = 0x1100
const HANGUL_L_END    = 0x115A
const HANGUL_L_FILLER = 0x115F
const HANGUL_V_START  = 0x1160
const HANGUL_V_END    = 0x11A3
const HANGUL_T_START  = 0x11A8
const HANGUL_T_END    = 0x11FA
const HANGUL_S_START  = 0xAC00
const HANGUL_S_END    = 0xD7A4


# Holds the value of a property.
const PropVal = Int16

# Struct containing information about a codepoint.
struct CharProperty
    category::PropVal 
    combining_class::PropVal 
    bidi_class::PropVal 
    decomp_type::PropVal 
    decomp_seqindex::UInt16 
    casefold_seqindex::UInt16 
    uppercase_seqindex::UInt16 
    lowercase_seqindex::UInt16 
    titlecase_seqindex::UInt16 
    comb_index::UInt16 
    flags::UInt16 
    # Flag bits (low to high bit)
    #   bidi_mirrored:1
    #   comp_exclusion:1
    #   ignorable:1
    #   control_boundary:1
    #   charwidth:2
    #   pad:2
    #   boundclass:6
    #   indic_conjunct_break:2
end

function CharProperty(category, combining_class, bidi_class, decomp_type,
        decomp_seqindex, casefold_seqindex, uppercase_seqindex, lowercase_seqindex,
        titlecase_seqindex, comb_index, bidi_mirrored, comp_exclusion, ignorable,
        control_boundary, charwidth, boundclass, indic_conjunct_break)
    flags = _pack_flags(bidi_mirrored, comp_exclusion, ignorable, control_boundary,
                        charwidth, boundclass, indic_conjunct_break)
    CharProperty(category, combining_class, bidi_class, decomp_type,
                 decomp_seqindex, casefold_seqindex, uppercase_seqindex,
                 lowercase_seqindex, titlecase_seqindex, comb_index, flags)
end

macro _pack_flag(flags, offset, flag, width)
    flagmsg = "$flag out of range"
    flag = esc(flag)
    mask = (1 << width) - 1
    width = esc(width)
    flags = esc(flags)
    offset = esc(offset)
    quote
        $flag & $mask == $flag || error($flagmsg)
        $flags |= $flag << $offset
        $offset += $width
    end
end

function _pack_flags(bidi_mirrored, comp_exclusion, ignorable, control_boundary,
                     charwidth, boundclass, indic_conjunct_break)
    flags = UInt16(0)
    bit_offset = 0
    @_pack_flag flags bit_offset bidi_mirrored        1
    @_pack_flag flags bit_offset comp_exclusion       1
    @_pack_flag flags bit_offset ignorable            1
    @_pack_flag flags bit_offset control_boundary     1
    @_pack_flag flags bit_offset charwidth            2
    @_pack_flag flags bit_offset 0                    2 # padding
    @_pack_flag flags bit_offset boundclass           6
    @_pack_flag flags bit_offset indic_conjunct_break 2
    flags
end

function Base.getproperty(cp::CharProperty, name::Symbol)
    name === :category             ? getfield(cp, :category)           :
    name === :combining_class      ? getfield(cp, :combining_class)    :
    name === :bidi_class           ? getfield(cp, :bidi_class)         :
    name === :decomp_type          ? getfield(cp, :decomp_type)        :
    name === :decomp_seqindex      ? getfield(cp, :decomp_seqindex)    :
    name === :casefold_seqindex    ? getfield(cp, :casefold_seqindex)  :
    name === :uppercase_seqindex   ? getfield(cp, :uppercase_seqindex) :
    name === :lowercase_seqindex   ? getfield(cp, :lowercase_seqindex) :
    name === :titlecase_seqindex   ? getfield(cp, :titlecase_seqindex) :
    name === :comb_index           ? getfield(cp, :comb_index)         :
    begin
        f = getfield(cp, :flags)
        name === :bidi_mirrored        ? (f & 0x01 == 0x00)            :
        name === :comp_exclusion       ? ((f >> 1)  & 0x01 == 0x00)    :
        name === :ignorable            ? ((f >> 2)  & 0x01 == 0x00)    :
        name === :control_boundary     ? ((f >> 3)  & 0x01 == 0x00)    :
        name === :charwidth            ? ((f >> 4)  & 0x03        )    :
        # name === :pad                  ? ((f >> 1) & 0x03        )   :
        name === :boundclass           ? ((f >> 8)  & 0x3f        )    :
        name === :indic_conjunct_break ? ((f >> 14) & 0x03        )    :
        error("No field")
    end
end

# Unicode categories.
const CATEGORY_CN  = 0 # Other, not assigned
const CATEGORY_LU  = 1 # Letter, uppercase
const CATEGORY_LL  = 2 # Letter, lowercase
const CATEGORY_LT  = 3 # Letter, titlecase
const CATEGORY_LM  = 4 # Letter, modifier
const CATEGORY_LO  = 5 # Letter, other
const CATEGORY_MN  = 6 # Mark, nonspacing
const CATEGORY_MC  = 7 # Mark, spacing combining
const CATEGORY_ME  = 8 # Mark, enclosing
const CATEGORY_ND  = 9 # Number, decimal digit
const CATEGORY_NL = 10 # Number, letter
const CATEGORY_NO = 11 # Number, other
const CATEGORY_PC = 12 # Punctuation, connector
const CATEGORY_PD = 13 # Punctuation, dash
const CATEGORY_PS = 14 # Punctuation, open
const CATEGORY_PE = 15 # Punctuation, close
const CATEGORY_PI = 16 # Punctuation, initial quote
const CATEGORY_PF = 17 # Punctuation, final quote
const CATEGORY_PO = 18 # Punctuation, other
const CATEGORY_SM = 19 # Symbol, math
const CATEGORY_SC = 20 # Symbol, currency
const CATEGORY_SK = 21 # Symbol, modifier
const CATEGORY_SO = 22 # Symbol, other
const CATEGORY_ZS = 23 # Separator, space
const CATEGORY_ZL = 24 # Separator, line
const CATEGORY_ZP = 25 # Separator, paragraph
const CATEGORY_CC = 26 # Other, control
const CATEGORY_CF = 27 # Other, format
const CATEGORY_CS = 28 # Other, surrogate
const CATEGORY_CO = 29 # Other, private use

# Bidirectional character classes.
# utf8proc_bidi_class_t
const BIDI_CLASS_L     = 1 # Left-to-Right
const BIDI_CLASS_LRE   = 2 # Left-to-Right Embedding
const BIDI_CLASS_LRO   = 3 # Left-to-Right Override
const BIDI_CLASS_R     = 4 # Right-to-Left
const BIDI_CLASS_AL    = 5 # Right-to-Left Arabic
const BIDI_CLASS_RLE   = 6 # Right-to-Left Embedding
const BIDI_CLASS_RLO   = 7 # Right-to-Left Override
const BIDI_CLASS_PDF   = 8 # Pop Directional Format
const BIDI_CLASS_EN    = 9 # European Number
const BIDI_CLASS_ES   = 10 # European Separator
const BIDI_CLASS_ET   = 11 # European Number Terminator
const BIDI_CLASS_AN   = 12 # Arabic Number
const BIDI_CLASS_CS   = 13 # Common Number Separator
const BIDI_CLASS_NSM  = 14 # Nonspacing Mark
const BIDI_CLASS_BN   = 15 # Boundary Neutral
const BIDI_CLASS_B    = 16 # Paragraph Separator
const BIDI_CLASS_S    = 17 # Segment Separator
const BIDI_CLASS_WS   = 18 # Whitespace
const BIDI_CLASS_ON   = 19 # Other Neutrals
const BIDI_CLASS_LRI  = 20 # Left-to-Right Isolate
const BIDI_CLASS_RLI  = 21 # Right-to-Left Isolate
const BIDI_CLASS_FSI  = 22 # First Strong Isolate
const BIDI_CLASS_PDI  = 23 # Pop Directional Isolate

# Decomposition type.
# utf8proc_decomp_type_t
const DECOMP_TYPE_FONT      = 1 # Font
const DECOMP_TYPE_NOBREAK   = 2 # Nobreak
const DECOMP_TYPE_INITIAL   = 3 # Initial
const DECOMP_TYPE_MEDIAL    = 4 # Medial
const DECOMP_TYPE_FINAL     = 5 # Final
const DECOMP_TYPE_ISOLATED  = 6 # Isolated
const DECOMP_TYPE_CIRCLE    = 7 # Circle
const DECOMP_TYPE_SUPER     = 8 # Super
const DECOMP_TYPE_SUB       = 9 # Sub
const DECOMP_TYPE_VERTICAL = 10 # Vertical
const DECOMP_TYPE_WIDE     = 11 # Wide
const DECOMP_TYPE_NARROW   = 12 # Narrow
const DECOMP_TYPE_SMALL    = 13 # Small
const DECOMP_TYPE_SQUARE   = 14 # Square
const DECOMP_TYPE_FRACTION = 15 # Fraction
const DECOMP_TYPE_COMPAT   = 16 # Compat

# Boundclass property. (TR29)
# utf8proc_boundclass_t
const BOUNDCLASS_START              =  0 # Start
const BOUNDCLASS_OTHER              =  1 # Other
const BOUNDCLASS_CR                 =  2 # Cr
const BOUNDCLASS_LF                 =  3 # Lf
const BOUNDCLASS_CONTROL            =  4 # Control
const BOUNDCLASS_EXTEND             =  5 # Extend
const BOUNDCLASS_L                  =  6 # L
const BOUNDCLASS_V                  =  7 # V
const BOUNDCLASS_T                  =  8 # T
const BOUNDCLASS_LV                 =  9 # Lv
const BOUNDCLASS_LVT                = 10 # Lvt
const BOUNDCLASS_REGIONAL_INDICATOR = 11 # Regional indicator
const BOUNDCLASS_SPACINGMARK        = 12 # Spacingmark
const BOUNDCLASS_PREPEND            = 13 # Prepend
const BOUNDCLASS_ZWJ                = 14 # Zero Width Joiner
# the following are no longer used in Unicode 11, but we keep
# the constants here for backward compatibility
const BOUNDCLASS_E_BASE             = 15 # Emoji Base
const BOUNDCLASS_E_MODIFIER         = 16 # Emoji Modifier
const BOUNDCLASS_GLUE_AFTER_ZWJ     = 17 # Glue_After_ZWJ
const BOUNDCLASS_E_BASE_GAZ         = 18 # E_BASE + GLUE_AFTER_ZJW
# the Extended_Pictographic property is used in the Unicode 11
# grapheme-boundary rules, so we store it in the boundclass field
const BOUNDCLASS_EXTENDED_PICTOGRAPHIC = 19
const BOUNDCLASS_E_ZWG = 20 # BOUNDCLASS_EXTENDED_PICTOGRAPHIC + ZWJ


# Indic_Conjunct_Break property. (TR44)
# utf8proc_indic_conjunct_break_t
const INDIC_CONJUNCT_BREAK_NONE = 0
const INDIC_CONJUNCT_BREAK_LINKER = 1
const INDIC_CONJUNCT_BREAK_CONSONANT = 2
const INDIC_CONJUNCT_BREAK_EXTEND = 3

# The utf8proc supported Unicode version as a string MAJOR.MINOR.PATCH.
const UNICODE_VERSION = v"15.1.0"

const _error_strings = [
    "Memory for processing UTF-8 data could not be allocated."
    "UTF-8 string is too long to be processed."
    "Invalid UTF-8 string"
    "Unassigned Unicode code point found in UTF-8 string."
    "Invalid options for UTF-8 processing chosen."
]

# Returns an informative error string for the given utf8proc error code
function errmsg(code)
    return code < 0 && code >= ERROR_INVALIDOPTS ?
        _error_strings[-code] :
        "An unknown error occurred while processing UTF-8 data."
end

#
# Reads a single codepoint from the UTF-8 sequence being pointed to by `str`.
# The maximum number of bytes read is `strlen`, unless `strlen` is
# negative (in which case up to 4 bytes are read).
#
# If a valid codepoint could be read, it is stored in the variable
# pointed to by `codepoint_ref`, otherwise that variable will be set to -1.
# In case of success, the number of bytes read is returned; otherwise, a
# negative error code is returned.
#
# utf8proc_ssize_t utf8proc_iterate(const utf8proc_uint8_t *str, utf8proc_ssize_t strlen, utf8proc_int32_t *codepoint_ref)
# See nextind()?

#
# Check if a codepoint is valid (regardless of whether it has been
# assigned a value by the current Unicode standard).
#
# @return 1 if the given `codepoint` is valid and otherwise return 0.
#
#utf8proc_bool utf8proc_codepoint_valid(utf8proc_int32_t codepoint)

#
# Encodes the codepoint as an UTF-8 string in the byte array pointed
# to by `dst`. This array must be at least 4 bytes long.
#
# In case of success the number of bytes written is returned, and
# otherwise 0 is returned.
#
# This function does not check whether `codepoint` is valid Unicode.
#
#utf8proc_ssize_t utf8proc_encode_char(utf8proc_int32_t codepoint, utf8proc_uint8_t *dst)

function unsafe_get_property(uc)
    # ASSERT: uc >= 0 && uc < 0x110000
    return @inbounds _properties[_stage2table[_stage1table[uc >> 8 + 1] + uc & 0xFF + 1] + 1]
end

#
# Look up the properties for a given codepoint.
#
# @param codepoint The Unicode codepoint.
#
# @returns
# A pointer to a (constant) struct containing information about
# the codepoint.
# @par
# If the codepoint is unassigned or invalid, a pointer to a special struct is
# returned in which `category` is 0 (@ref CATEGORY_CN).
#
function get_property(uc)
    return uc < 0 || uc >= 0x110000 ? _properties[1] : unsafe_get_property(uc);
end

# NB: `i` should be decoded to be 1-based at this point.
function _seqindex_decode_entry(i)
    entry_cp = UInt32(_sequences[i])
    if (entry_cp & 0xF800) == 0xD800
        i += 1
        entry_cp = ((entry_cp & 0x03FF) << 10) | (_sequences[i] & 0x03FF)
        entry_cp += 0x10000
    end
    return entry_cp, i + 1
end

function _seqindex_decode_index(seqindex)
    ch, _ = _seqindex_decode_entry(seqindex + 1)
    return ch
end

function _seqindex_write_char_decomposed(seqindex::UInt16, dst::Ptr{UInt32},
                                         bufsize, options, last_boundclass)
    written = 0
    entry_idx = seqindex & 0x3FFF + 1
    len = seqindex >> 14
    if len >= 3
        len = _sequences[entry_idx]
        entry_idx += 1
    end
    while len >= 0
        entry_cp, entry_idx = _seqindex_decode_entry(entry_idx)

        written += decompose_char(entry_cp, dst + written * sizeof(UInt32),
                                  (bufsize > written) ? (bufsize - written) : 0, options,
                                  last_boundclass)
        if written < 0
            return ERROR_OVERFLOW
        end
        len -= 1
    end
    return written
end

# julia> using StatsBase
#        dst=zeros(UInt32,100)
#        decomps = [(n=UTF8Proc.decompose_char(uc, pointer(dst), 100, UTF8Proc.DECOMPOSE, Ref{Int32}(0)); dst[1:n]) for uc in 0x00000000:0x0010ffff]
#        countmap(length.(decomps))
# Dict{Int64, Int64} with 4 entries:
#   4 => 36
#   2 => 1178
#   3 => 11002
#   1 => 1101896
const MAX_DECOMPOSE_CODEPOINTS = 4

"""
Decompose a codepoint into an array of codepoints.

* `codepoint` the codepoint.
* `dst` the destination buffer.
* `bufsize` the size of the destination buffer.
* `options` one or more of the following flags:
    - `REJECTNA` - return an error `codepoint` is unassigned
    - `IGNORE`   - strip "default ignorable" codepoints
    - `CASEFOLD` - apply Unicode casefolding
    - `COMPAT`   - replace certain codepoints with their
                                compatibility decomposition
    - `CHARBOUND`- insert 0xFF bytes before each grapheme cluster
    - `LUMP`     - lump certain different codepoints together
    - `STRIPMARK`- remove all character marks
    - `STRIPNA`  - remove unassigned codepoints
* `last_boundclass`
    Pointer to an integer variable containing
    the previous codepoint's (boundclass + indic_conjunct_break << 1) if the `CHARBOUND`
    option is used.  If the string is being processed in order, this can be initialized to 0 for
    the beginning of the string, and is thereafter updated automatically.  Otherwise, this parameter is ignored.

In case of success, return the number of codepoints written is returned; in
case of an error, a negative error code is returned (utf8proc_errmsg()).

If the number of written codepoints would be bigger than `bufsize`, the
required buffer size is returned, while the buffer will be overwritten with
undefined data.
"""
function decompose_char(uc::UInt32, dst::Ptr{UInt32}, bufsize, options, last_boundclass)
    if uc < 0 || uc >= 0x110000
        return ERROR_NOTASSIGNED
    end
    property = unsafe_get_property(uc)
    category = property.category

    if options & (COMPOSE|DECOMPOSE) != 0
        hangul_sindex = uc - HANGUL_SBASE
        if hangul_sindex >= 0 && hangul_sindex < HANGUL_SCOUNT
            if bufsize >= 1
                unsafe_store!(dst, HANGUL_LBASE + div(hangul_sindex, HANGUL_NCOUNT), 1)
                if bufsize >= 2
                    unsafe_store!(dst, HANGUL_VBASE + div((hangul_sindex % HANGUL_NCOUNT), HANGUL_TCOUNT), 2)
                end
            end
            hangul_tindex = hangul_sindex % HANGUL_TCOUNT
            if hangul_tindex == 0
                return 2
            end
            if bufsize >= 3
                unsafe_store!(dst, HANGUL_TBASE + hangul_tindex, 3)
            end
            return 3
        end
    end
    if (options & REJECTNA != 0) && category == 0
        return ERROR_NOTASSIGNED
    end
    if (options & IGNORE != 0) && property.ignorable
        return 0
    end
    if (options & STRIPNA != 0) && category == 0
        return 0
    end

    if options & LUMP != 0
        replacement_uc =
            category == CATEGORY_ZS                                       ? 0x0020 :
            uc == 0x2018 || uc == 0x2019 || uc == 0x02BC || uc == 0x02C8  ? 0x0027 :
            category == CATEGORY_PD || uc == 0x2212                       ? 0x002D :
            uc == 0x2044 || uc == 0x2215                                  ? 0x002F :
            uc == 0x2236                                                  ? 0x003A :
            uc == 0x2039 || uc == 0x2329 || uc == 0x3008                  ? 0x003C :
            uc == 0x203A || uc == 0x232A || uc == 0x3009                  ? 0x003E :
            uc == 0x2216                                                  ? 0x005C :
            uc == 0x02C4 || uc == 0x02C6 || uc == 0x2038 || uc == 0x2303  ? 0x005E :
            category == CATEGORY_PC || uc == 0x02CD                       ? 0x005F :
            uc == 0x02CB                                                  ? 0x0060 :
            uc == 0x2223                                                  ? 0x007C :
            uc == 0x223C                                                  ? 0x007E :
            (options & NLF2LS != 0) && (options & NLF2PS != 0) &&
              (category == CATEGORY_ZL || category == CATEGORY_ZP)        ? 0x000A :
            0x0000
        if replacement_uc != 0x0000
            return decompose_char(replacement_uc, dst, bufsize,
                                  options & ~UInt32(LUMP), last_boundclass)
        end
    end

    if options & STRIPMARK != 0
        if (category == CATEGORY_MN || category == CATEGORY_MC || category == CATEGORY_ME)
            return 0
        end
    end
    if options & CASEFOLD != 0
        if property.casefold_seqindex != typemax(UInt16)
            return _seqindex_write_char_decomposed(property.casefold_seqindex, dst, bufsize, options, last_boundclass)
        end
    end
    if options & (COMPOSE|DECOMPOSE) != 0
        if property.decomp_seqindex != typemax(UInt16) &&
            (property.decomp_type == 0 || (options & COMPAT != 0))
            return _seqindex_write_char_decomposed(property.decomp_seqindex, dst, bufsize, options, last_boundclass)
        end
    end
    if options & CHARBOUND != 0
        boundary, last_boundclass[] =
            _grapheme_break_extended(0, property.boundclass, 0, property.indic_conjunct_break,
                                     last_boundclass[])
        if boundary
            if bufsize >= 1
                unsafe_store!(dst, -1, 1) # sentinel value for grapheme break
            end
            if bufsize >= 2
                unsafe_store!(dst, uc, 2)
            end
            return 2
        end
    end
    if bufsize >= 1
        unsafe_store!(dst, uc, 1)
    end
    return 1
end


#
 # The same as utf8proc_decompose_char(), but acts on a whole UTF-8
 # string and orders the decomposed sequences correctly.
 #
 # If the @ref NULLTERM flag in `options` is set, processing
 # will be stopped, when a NULL byte is encountered, otherwise `strlen`
 # bytes are processed.  The result (in the form of 32-bit unicode
 # codepoints) is written into the buffer being pointed to by
 # `buffer` (which must contain at least `bufsize` entries).  In case of
 # success, the number of codepoints written is returned; in case of an
 # error, a negative error code is returned (utf8proc_errmsg()).
 # See utf8proc_decompose_custom() to supply additional transformations.
 #
 # If the number of written codepoints would be bigger than `bufsize`, the
 # required buffer size is returned, while the buffer will be overwritten with
 # undefined data.
 #
# utf8proc_ssize_t utf8proc_decompose(
#   const utf8proc_uint8_t *str, utf8proc_ssize_t strlen,
#   utf8proc_int32_t *buffer, utf8proc_ssize_t bufsize, utf8proc_option_t options
# )
# ^ TODO
function utf8proc_decompose(s::AbstractString, options)
    charbuf_ref = Ref{NTuple{MAX_DECOMPOSE_CODEPOINTS,UInt32}}()
    charbuf = Ptr{UInt32}(Base.pointer_from_objref(charbuf_ref))
    last_boundclass = Ref{Int32}(0)
    io = IOBuffer()
    GC.@preserve charbuf_ref begin
        for c in s
            if isvalid(c)
                uc = UInt32(c)
                n = decompose_char(uc, charbuf, MAX_DECOMPOSE_CODEPOINTS, options, last_boundclass)
                @assert n <= MAX_DECOMPOSE_CODEPOINTS
                for i = 1:n
                    write(io, Char(unsafe_load(charbuf, i)))
                end
            else
                last_boundclass[] = 0
                write(io, c)
            end
        end
    end
    String(take!(io))
end

#
 # The same as utf8proc_decompose(), but also takes a `custom_func` mapping function
 # that is called on each codepoint in `str` before any other transformations
 # (along with a `custom_data` pointer that is passed through to `custom_func`).
 # The `custom_func` argument is ignored if it is `NULL`.  See also utf8proc_map_custom().
 #
# utf8proc_ssize_t utf8proc_decompose_custom(
#   const utf8proc_uint8_t *str, utf8proc_ssize_t strlen,
#   utf8proc_int32_t *buffer, utf8proc_ssize_t bufsize, utf8proc_option_t options,
#   utf8proc_custom_func custom_func, void *custom_data
#)
# ^ TODO

#
 # Normalizes the sequence of `length` codepoints pointed to by `buffer`
 # in-place (i.e., the result is also stored in `buffer`).
 #
 # @param buffer the (native-endian UTF-32) unicode codepoints to re-encode.
 # @param length the length (in codepoints) of the buffer.
 # @param options a bitwise or (`|`) of one or more of the following flags:
 # - @ref NLF2LS  - convert LF, CRLF, CR and NEL into LS
 # - @ref NLF2PS  - convert LF, CRLF, CR and NEL into PS
 # - @ref NLF2LF  - convert LF, CRLF, CR and NEL into LF
 # - @ref STRIPCC - strip or convert all non-affected control characters
 # - @ref COMPOSE - try to combine decomposed codepoints into composite
 #                           codepoints
 # - @ref STABLE  - prohibit combining characters that would violate
 #                           the unicode versioning stability
 #
 # @return
 # In case of success, the length (in codepoints) of the normalized UTF-32 string is
 # returned; otherwise, a negative error code is returned (utf8proc_errmsg()).
 #
 # @warning The entries of the array pointed to by `str` have to be in the
 #          range `0x0000` to `0x10FFFF`. Otherwise, the program might crash!
 #
# utf8proc_ssize_t utf8proc_normalize_utf32(utf8proc_int32_t *buffer, utf8proc_ssize_t length, utf8proc_option_t options)

#
 # Reencodes the sequence of `length` codepoints pointed to by `buffer`
 # UTF-8 data in-place (i.e., the result is also stored in `buffer`).
 # Can optionally normalize the UTF-32 sequence prior to UTF-8 conversion.
 #
 # @param buffer the (native-endian UTF-32) unicode codepoints to re-encode.
 # @param length the length (in codepoints) of the buffer.
 # @param options a bitwise or (`|`) of one or more of the following flags:
 # - @ref NLF2LS  - convert LF, CRLF, CR and NEL into LS
 # - @ref NLF2PS  - convert LF, CRLF, CR and NEL into PS
 # - @ref NLF2LF  - convert LF, CRLF, CR and NEL into LF
 # - @ref STRIPCC - strip or convert all non-affected control characters
 # - @ref COMPOSE - try to combine decomposed codepoints into composite
 #                           codepoints
 # - @ref STABLE  - prohibit combining characters that would violate
 #                           the unicode versioning stability
 # - @ref CHARBOUND - insert 0xFF bytes before each grapheme cluster
 #
 # @return
 # In case of success, the length (in bytes) of the resulting nul-terminated
 # UTF-8 string is returned; otherwise, a negative error code is returned
 # (utf8proc_errmsg()).
 #
 # @warning The amount of free space pointed to by `buffer` must
 #          exceed the amount of the input data by one byte, and the
 #          entries of the array pointed to by `str` have to be in the
 #          range `0x0000` to `0x10FFFF`. Otherwise, the program might crash!
 #
#utf8proc_ssize_t utf8proc_reencode(utf8proc_int32_t *buffer, utf8proc_ssize_t length, utf8proc_option_t options)
# ^ TODO

# return whether there is a grapheme break between boundclasses lbc and tbc
#  (according to the definition of extended grapheme clusters)
#
# Rule numbering refers to TR29 Version 29 (Unicode 9.0.0):
# http://www.unicode.org/reports/tr29/tr29-29.html
#
# CAVEATS:
#  Please note that evaluation of GB10 (grapheme breaks between emoji zwj sequences)
#  and GB 12/13 (regional indicator code points) require knowledge of previous characters
#  and are thus not handled by this function. This may result in an incorrect break before
#  an E_Modifier class codepoint and an incorrectly missing break between two
#  REGIONAL_INDICATOR class code points if such support does not exist in the caller.
#
#  See the special support in _grapheme_break_extended, for required bookkeeping by the caller.
#
function _grapheme_break_simple(lbc, tbc)
    (lbc == BOUNDCLASS_START) ? true :       # GB1
    (lbc == BOUNDCLASS_CR &&                 # GB3
     tbc == BOUNDCLASS_LF) ? false :         # ---
    (lbc >= BOUNDCLASS_CR && lbc <= BOUNDCLASS_CONTROL) ? true :  # GB4
    (tbc >= BOUNDCLASS_CR && tbc <= BOUNDCLASS_CONTROL) ? true :  # GB5
    (lbc == BOUNDCLASS_L &&                  # GB6
     (tbc == BOUNDCLASS_L ||                 # ---
      tbc == BOUNDCLASS_V ||                 # ---
      tbc == BOUNDCLASS_LV ||                # ---
      tbc == BOUNDCLASS_LVT)) ? false :      # ---
    ((lbc == BOUNDCLASS_LV ||                # GB7
      lbc == BOUNDCLASS_V) &&                # ---
     (tbc == BOUNDCLASS_V ||                 # ---
      tbc == BOUNDCLASS_T)) ? false :        # ---
    ((lbc == BOUNDCLASS_LVT ||               # GB8
      lbc == BOUNDCLASS_T) &&                # ---
     tbc == BOUNDCLASS_T) ? false :          # ---
    (tbc == BOUNDCLASS_EXTEND ||             # GB9
     tbc == BOUNDCLASS_ZWJ ||                # ---
     tbc == BOUNDCLASS_SPACINGMARK ||        # GB9a
     lbc == BOUNDCLASS_PREPEND) ? false :    # GB9b
    (lbc == BOUNDCLASS_E_ZWG &&              # GB11 (requires additional handling below)
     tbc == BOUNDCLASS_EXTENDED_PICTOGRAPHIC) ? false : # ----
    (lbc == BOUNDCLASS_REGIONAL_INDICATOR &&          # GB12/13 (requires additional handling below)
     tbc == BOUNDCLASS_REGIONAL_INDICATOR) ? false :  # ----
    true # GB999
end

function _grapheme_break_extended(lbc, tbc, licb, ticb, state)
    # boundclass and indic_conjunct_break state
    state_bc::UInt32 = 0
    state_icb::UInt32 = 0
    if state == 0 # state initialization
        state_bc = lbc
        state_icb = licb == INDIC_CONJUNCT_BREAK_CONSONANT ? licb : INDIC_CONJUNCT_BREAK_NONE
    else # lbc and licb are already encoded in state
        state_bc = state & 0xff  # 1st byte of state is bound class
        state_icb = state >> 8   # 2nd byte of state is indic conjunct break
    end

    break_permitted = _grapheme_break_simple(state_bc, tbc) &&
       !(state_icb == INDIC_CONJUNCT_BREAK_LINKER
        && ticb == INDIC_CONJUNCT_BREAK_CONSONANT) # GB9c

    # Special support for GB9c.  Don't break between two consonants
    # separated 1+ linker characters and 0+ extend characters in any order.
    # After a consonant, we enter LINKER state after at least one linker.
    if (ticb == INDIC_CONJUNCT_BREAK_CONSONANT
        || state_icb == INDIC_CONJUNCT_BREAK_CONSONANT
        || state_icb == INDIC_CONJUNCT_BREAK_EXTEND)
        state_icb = ticb
    elseif (state_icb == INDIC_CONJUNCT_BREAK_LINKER)
        state_icb = ticb == INDIC_CONJUNCT_BREAK_EXTEND ?
                    INDIC_CONJUNCT_BREAK_LINKER : ticb
    end

    # Special support for GB 12/13 made possible by GB999. After two RI
    # class codepoints we want to force a break. Do this by resetting the
    # second RI's bound class to BOUNDCLASS_OTHER, to force a break
    # after that character according to GB999 (unless of course such a break is
    # forbidden by a different rule such as GB9).
    if (state_bc == tbc && tbc == BOUNDCLASS_REGIONAL_INDICATOR)
        state_bc = BOUNDCLASS_OTHER
        # Special support for GB11 (emoji extend* zwj / emoji)
    elseif (state_bc == BOUNDCLASS_EXTENDED_PICTOGRAPHIC)
        if (tbc == BOUNDCLASS_EXTEND) # fold EXTEND codepoints into emoji
            state_bc = BOUNDCLASS_EXTENDED_PICTOGRAPHIC
        elseif (tbc == BOUNDCLASS_ZWJ)
            state_bc = BOUNDCLASS_E_ZWG; # state to record emoji+zwg combo
        else
            state_bc = tbc
        end
    else
        state_bc = tbc
    end

    return (break_permitted, state_bc + (state_icb << 8))
end

# Given a pair of consecutive codepoints, return whether a grapheme break is
# permitted between them (as defined by the extended grapheme clusters in UAX#29).
#
# @param codepoint1 The first codepoint.
# @param codepoint2 The second codepoint, occurring consecutively after `codepoint1`.
# @param state Beginning with Version 29 (Unicode 9.0.0), this algorithm requires
#              state to break graphemes. This state can be passed in as a pointer
#              in the `state` argument and should initially be set to 0. If the
#              state is not passed in (i.e. a null pointer is passed), UAX#29 rules
#              GB10/12/13 which require this state will not be applied, essentially
#              matching the rules in Unicode 8.0.0.
#
# @warning If the state parameter is used, `utf8proc_grapheme_break_stateful` must
#          be called IN ORDER on ALL potential breaks in a string.  However, it
#          is safe to reset the state to zero after a grapheme break.
#
function grapheme_break_stateful(c1::UInt32, c2::UInt32, state::Ref{Int32})
    p1 = get_property(c1)
    p2 = get_property(c2)
    break_permitted, newstate =
        _grapheme_break_extended(p1.boundclass, p2.boundclass,
                                 p1.indic_conjunct_break, p2.indic_conjunct_break,
                                 state[])
    state[] = newstate
    return break_permitted
end

# Given a codepoint `c`, return the codepoint of the corresponding
# lower-case character, if any; otherwise (if there is no lower-case
# variant, or if `c` is not a valid codepoint) return `c`.
function tolower(c::UInt32)
    cl = get_property(c).lowercase_seqindex
    return cl != typemax(UInt16) ? _seqindex_decode_index(cl) : c
end

# Given a codepoint `c`, return the codepoint of the corresponding
# upper-case character, if any; otherwise (if there is no upper-case
# variant, or if `c` is not a valid codepoint) return `c`.
function toupper(c::UInt32)
    cu = get_property(c).uppercase_seqindex
    return cu != typemax(UInt16) ? _seqindex_decode_index(cu) : c
end

# Given a codepoint `c`, return the codepoint of the corresponding
# title-case character, if any; otherwise (if there is no title-case
# variant, or if `c` is not a valid codepoint) return `c`.
function totitle(c::UInt32)
    cu = get_property(c).titlecase_seqindex
    return cu != typemax(UInt16) ? _seqindex_decode_index(cu) : c
end

# Given a codepoint `c`, return `1` if the codepoint corresponds to a lower-case character
# and `0` otherwise.
function islower(c::UInt32)
    p = get_property(c)
    return p.lowercase_seqindex != p.uppercase_seqindex && p.lowercase_seqindex == typemax(UInt16)
end

# Given a codepoint `c`, return `1` if the codepoint corresponds to an upper-case character
# and `0` otherwise.
function isupper(c::UInt32)
    p = get_property(c)
    return p.lowercase_seqindex != p.uppercase_seqindex && p.uppercase_seqindex == typemax(UInt16) && p.category != CATEGORY_LT
end

# Given a codepoint, return a character width analogous to `wcwidth(codepoint)`,
# except that a width of 0 is returned for non-printable codepoints
# instead of -1 as in `wcwidth`.
#
# @note
# If you want to check for particular types of non-printable characters,
# (analogous to `isprint` or `iscntrl`), use category().
function charwidth(c::UInt32)
    return Int(get_property(c).charwidth)
end

# Return the Unicode category for the codepoint (one of the
# @ref utf8proc_category_t constants.)
function category(c::UInt32)
    return get_property(c).category
end


#
 # Maps the given UTF-8 string pointed to by `str` to a new UTF-8
 # string, allocated dynamically by `malloc` and returned via `dstptr`.
 #
 # If the @ref NULLTERM flag in the `options` field is set,
 # the length is determined by a NULL terminator, otherwise the
 # parameter `strlen` is evaluated to determine the string length, but
 # in any case the result will be NULL terminated (though it might
 # contain NULL characters with the string if `str` contained NULL
 # characters). Other flags in the `options` field are passed to the
 # functions defined above, and regarded as described.  See also
 # utf8proc_map_custom() to supply a custom codepoint transformation.
 #
 # In case of success the length of the new string is returned,
 # otherwise a negative error code is returned.
 #
 # @note The memory of the new UTF-8 string will have been allocated
 # with `malloc`, and should therefore be deallocated with `free`.
 #
#utf8proc_ssize_t utf8proc_map(
#  const utf8proc_uint8_t *str, utf8proc_ssize_t strlen, utf8proc_uint8_t **dstptr, utf8proc_option_t options
#)
# ^TODO

#
 # Like utf8proc_map(), but also takes a `custom_func` mapping function
 # that is called on each codepoint in `str` before any other transformations
 # (along with a `custom_data` pointer that is passed through to `custom_func`).
 # The `custom_func` argument is ignored if it is `NULL`.
 #
# utf8proc_ssize_t utf8proc_map_custom(
#   const utf8proc_uint8_t *str, utf8proc_ssize_t strlen, utf8proc_uint8_t **dstptr, utf8proc_option_t options,
#   utf8proc_custom_func custom_func, void *custom_data
# )

# @name Unicode normalization
 #
 # Returns a pointer to newly allocated memory of a NFD, NFC, NFKD, NFKC or
 # NFKC_Casefold normalized version of the null-terminated string `str`.  These
 # are shortcuts to calling utf8proc_map() with @ref NULLTERM
 # combined with @ref STABLE and flags indicating the normalization.
 #
# @{
# NFD normalization (@ref DECOMPOSE).
#utf8proc_uint8_t *utf8proc_NFD(const utf8proc_uint8_t *str)
# NFC normalization (@ref COMPOSE).
#utf8proc_uint8_t *utf8proc_NFC(const utf8proc_uint8_t *str)
# NFKD normalization (@ref DECOMPOSE and @ref COMPAT).
#utf8proc_uint8_t *utf8proc_NFKD(const utf8proc_uint8_t *str)
# NFKC normalization (@ref COMPOSE and @ref COMPAT).
#utf8proc_uint8_t *utf8proc_NFKC(const utf8proc_uint8_t *str)
#
 # NFKC_Casefold normalization (@ref COMPOSE and @ref COMPAT
 # and @ref CASEFOLD and @ref IGNORE).
 #
#utf8proc_uint8_t *utf8proc_NFKC_Casefold(const utf8proc_uint8_t *str)
# @}

include("data.jl")

