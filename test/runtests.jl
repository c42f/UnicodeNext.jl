using UnicodeNext
using Test

using UnicodeNext: textwidth, lowercase, uppercase, titlecase,
    category_abbrev, category_string, isassigned, isuppercase, islowercase, iscased,
    isdigit, isletter, isnumeric, iscntrl, ispunct, isspace, isprint, isxdigit,
    uppercase, lowercase, titlecase, uppercasefirst, lowercasefirst,
    GraphemeState, isgraphemebreak, graphemes, isequal_normalized, normalize

@testset "Basic smoke tests" begin
    @test textwidth('a') == 1
    @test textwidth('α') == 1
    @test textwidth('⛵') == 2
    @test textwidth("aα⛵") == 4

    @test lowercase('A') == 'a'
    @test lowercase('Ö') == 'ö'

    @test uppercase('a') == 'A'
    @test uppercase('ê') == 'Ê'

    @test titlecase('a') == 'A'
    @test titlecase('ǆ') == 'ǅ'
    @test uppercase('ǆ') == 'Ǆ'

    # TODO: Do we want category_abbrev, category_string, isassigned to be public?
    @test category_abbrev('a') == "Ll"
    @test category_abbrev('1') == "Nd"
    @test category_abbrev('₁') == "No"
    @test category_abbrev(' ') == "Zs"
    @test category_abbrev('\n') == "Cc"

    @test category_string('a') == "Letter, lowercase"
    @test category_string('\n') == "Other, control"

    @test isassigned(101) == true
    @test isassigned('a') == true
    @test isassigned('\u378') == false

    @test islowercase('α') == true
    @test islowercase('Γ') == false
    @test islowercase('❤') == false

    @test isuppercase('γ') == false
    @test isuppercase('Γ') == true
    @test isuppercase('❤') == false

    @test iscased('a') == true
    @test iscased('❤') == false

    @test isdigit('1') == true
    @test isdigit('₁') == false
    @test isdigit('a') == false

    @test isletter('❤') == false
    @test isletter('α') == true
    @test isletter('9') == false

    @test isnumeric('₁') == true
    @test isnumeric('௰') == true
    @test isnumeric('9') == true
    @test isnumeric('α') == false
    @test isnumeric('❤') == false

    @test iscntrl('\x01') == true
    @test iscntrl('a') == false

    @test ispunct('α') == false
    @test ispunct('/') == true
    @test ispunct(';') == true

    @test isspace('\n') == true
    @test isspace('\r') == true
    @test isspace(' ') == true
    @test isspace('\x20') == true

    @test isprint('\x01') == false
    @test isprint('A') == true

    @test isxdigit('a') == true
    @test isxdigit('x') == false

    @test uppercase("Julia") == "JULIA"
    @test lowercase("STRINGS AND THINGS") == "strings and things"

    @test titlecase("the JULIA programming language") ==
        "The Julia Programming Language"
    @test titlecase("ISS - international space station", strict=false) ==
        "ISS - International Space Station"
    @test titlecase("a-a b-b", wordsep = c->c==' ') ==
        "A-a B-b"

    @test uppercasefirst("python") == "Python"

    @test lowercasefirst("Julia") == "julia"

    @test collect(graphemes("a🏳️‍🌈b")) == [
        "a"
        "🏳️\u200d🌈"
        "b"
    ]
    @test graphemes("exposé", 3:6) == "posé"

    # FIXME: This stateful API sucks. Why do we need to provide both characters?
    gs = GraphemeState()
    # Following is the codepoint breakdown of 🏳️‍🌈
    @test begin gs, brk = isgraphemebreak(gs, 'a');       brk == true   end
    @test begin gs, brk = isgraphemebreak(gs, '\U1F3F3'); brk == true   end
    @test begin gs, brk = isgraphemebreak(gs, '\ufe0f');  brk == false  end
    @test begin gs, brk = isgraphemebreak(gs, '\u200d');  brk == false  end
    @test begin gs, brk = isgraphemebreak(gs, '🌈');      brk == false  end
    @test begin gs, brk = isgraphemebreak(gs, 'b');       brk == true   end

    s1 = "no\u00EBl"
    s2 = "noe\u0308l"
    @test s1 != s2
    @test isequal_normalized(s1, s2)
    @test isequal_normalized(s1, "noel", stripmark=true)
    @test isequal_normalized(s1, "NOËL", casefold=true)

    @test normalize("\u0065\u0301") == "\u00e9" # é
    @test normalize("\u0065\u0301", compose=false) == "\u0065\u0301" # é
    @test normalize("\u00e9", decompose=true) == "\u0065\u0301" # é
    # (de)composition with codes > \uffff
    @test normalize("\U1d158\U1d165\U1d16e") == "\U1d160"  # 𝅘𝅥𝅮
    @test normalize("\U1d160", decompose=true) == "\U1d158\U1d165\U1d16e"  # 𝅘𝅥𝅮
    # Hangul composition special cases
    @test normalize("\U1100\U1161\U11A8") == "\UAC01" # 각
    @test normalize("\UAC01", decompose=true) == "\U1100\U1161\U11A8" # 각

    @test normalize("\u00b5", compat=true) == "\u03bc" # μ
    @test normalize("\u00b5", compat=false) == "\u00b5" # μ

    @test normalize("JuLiA", casefold=true) == "julia"

    @test normalize("\r\n \n \r \u0085", newline2ls=true) == "\u2028 \u2028 \u2028 \u2028"
    @test normalize("\r\n \n \r \u0085", newline2ps=true) == "\u2029 \u2029 \u2029 \u2029"
    @test normalize("\r\n \n \r \u0085", newline2lf=true) == "\n \n \n \n"
    @test normalize("\r\n \n \r \u0085", newline2lf=true) == "\n \n \n \n"
    @test normalize("-a-\0-b-\r\n-c-\n-d-\r-e-\u0085-f-\t-g-", stripcc=true) ==
        "-a--b- -c- -d- -e- -f- -g-"
    @test normalize("JúLiA", stripmark=true) == "JuLiA"
    @test normalize(".\u00ad.", stripignore=true) == ".."
    @test normalize("a", rejectna=true) == "a"
    @test_throws ErrorException("Unassigned Unicode code point found in UTF-8 string.") #=
        =# normalize("\Ue1000", rejectna=true)
    # Lump all space-like codepoints together, etc
    @test normalize("\ua0", lump=true) == " "
end

# TODO:
# * Port utf8proc tests?
# * Port Base tests?

include("graphemebreak.jl")
