using UnicodeNext: graphemes

function read_grapheme_break_samples(filename)
    samples = Vector{String}[]
    for line in readlines(filename)
        line = replace(line, r"\s*(#.*)?$"=>"")
        isempty(line) && continue
        line = strip(line, '÷')
        clusters = map(split(line, '÷')) do cluster
            isempty(strip(cluster)) ?
                "" :
                join(Char.(parse.(UInt32, strip.(split(cluster, '×')), base=16)))
        end
        push!(samples, clusters)
    end
    return samples
end

@testset "Grapheme breaks" begin
    test_data_path = joinpath(@__DIR__, "GraphemeBreakTest_$(UnicodeNext.UNICODE_VERSION).txt")
    if !isfile(test_data_path)
        download("https://www.unicode.org/Public/$(UnicodeNext.UNICODE_VERSION)/ucd/auxiliary/GraphemeBreakTest.txt", test_data_path)
    end
    samples = read_grapheme_break_samples(test_data_path)
    @testset "$(repr(join(clusters)))" for clusters in samples
        str = join(clusters)
        @test collect(graphemes(str)) == clusters
        # Also test range-based version
        n = length(clusters)
        for i=1:n
            r1 = i:n
            @test graphemes(str, r1) == join(clusters[r1])
            r2 = 1:n-i+1
            @test graphemes(str, r2) == join(clusters[r2])
        end
    end
end
