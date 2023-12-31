# Unicode data generation rules.  Except for the test data files, most
# users will not use these Makefile rules, which are primarily to re-generate
# data.jl when we get a new Unicode version or charwidth data

# programs
CURL=curl
JULIA=julia
CURLFLAGS = --retry 5 --location

.PHONY: clean rawdata

.DELETE_ON_ERROR:

data.new.jl: data_generator.jl rawdata
	$(JULIA) --project=. data_generator.jl Julia > $@

# Unicode data version (must also update utf8proc_unicode_version function)
UNICODE_VERSION=15.1.0

rawdata: UnicodeData.txt GraphemeBreakProperty.txt DerivedCoreProperties.txt CompositionExclusions.txt CaseFolding.txt EastAsianWidth.txt emoji-data.txt

UnicodeData.txt:
	$(CURL) $(CURLFLAGS) -o $@ https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/UnicodeData.txt

EastAsianWidth.txt:
	$(CURL) $(CURLFLAGS) -o $@ $(URLCACHE)https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/EastAsianWidth.txt

GraphemeBreakProperty.txt:
	$(CURL) $(CURLFLAGS) -o $@ $(URLCACHE)https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/auxiliary/GraphemeBreakProperty.txt

DerivedCoreProperties.txt:
	$(CURL) $(CURLFLAGS) -o $@ $(URLCACHE)https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/DerivedCoreProperties.txt

CompositionExclusions.txt:
	$(CURL) $(CURLFLAGS) -o $@ $(URLCACHE)https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/CompositionExclusions.txt

CaseFolding.txt:
	$(CURL) $(CURLFLAGS) -o $@ $(URLCACHE)https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/CaseFolding.txt

NormalizationTest.txt:
	$(CURL) $(CURLFLAGS) -o $@ $(URLCACHE)https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/NormalizationTest.txt

GraphemeBreakTest.txt:
	$(CURL) $(CURLFLAGS) -o $@ $(URLCACHE)https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/auxiliary/GraphemeBreakTest.txt

emoji-data.txt:
	$(CURL) $(CURLFLAGS) -o $@ $(URLCACHE)https://unicode.org/Public/$(UNICODE_VERSION)/ucd/emoji/emoji-data.txt

clean:
	rm -f UnicodeData.txt EastAsianWidth.txt GraphemeBreakProperty.txt DerivedCoreProperties.txt CompositionExclusions.txt CaseFolding.txt NormalizationTest.txt GraphemeBreakTest.txt emoji-data.txt
	rm -f data.new.jl
