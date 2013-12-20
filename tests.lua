local strung = require"strung"

--- First, the test and benchmark infrastructure.
--- `try` and `gmtry` run both the original and strung version of the functions, 
--- and compare either their ouput, or their speed if "bench" is passed as a 
--- parameter to the script.

local ttstr = require"util".val_to_string
local BASE = 10000
local iter, try, gmtry
if arg[1] == "bench" then 
    function try(f, a, s, d, g, h)
        -- jit.off() jit.on()
        tsi, tSo = 0, 0
        local params = {a, s, d, g, h}
        local ri, Ros
        print(("-_"):rep(30))
        print("Test: ", f, unpack(params))
        local tic = os.clock()
        for i = 1, II do
            ri = {string[f](a, s, d, g, h)}
        end
        tsi = os.clock() - tic
        local tic = os.clock()
        for i = 1, II do
            Ro = {strung[f](a, s, d, g, h)}
        end
        tSo = os.clock() - tic
        print("strung/string: ", tSo/tsi)
    end
    function gmtry(s, p)
        print(("-_"):rep(30))
        print("Test: ", "gmatch", s, p)
        local ri, ro = {}, {}
        local tic = os.clock()
        for i = 1, II do
            ro = {}
            for a, b, c, d, e, f in strung.gmatch(s, p) do
                ro[#ro + 1] = {a, b, c, d, e}
            end
        end
        local tsi = os.clock() - tic
        local tic = os.clock()
        for i = 1, II do
            ri = {}
            for a, b, c, d, e, f in string.gmatch(s, p) do
                ri[#ri + 1] = {a, b, c, d, e}
            end
        end
        local tSo = os.clock() - tic
        print("strung/string: ", tSo/tsi)
    end
    function iter(n) II = BASE * n end
    iter(10)
else
    function try(f, ...)
        local params = {...}
        local ri, Ros
        -- print(("-_"):rep(30))
        -- print("Test: ", f, ...)
        ri = {string[f](...)}
        Ro = {strung[f](...)}
        for i, v in ipairs(params) do params[i] = tostring(v) end
        for i = 1, math.max(#ri, #Ro) do
            strung.assert(ri[i] == Ro[i], params[2], table.concat({ 
                table.concat(params, ", "), 
                "ri:", table.concat(ri, ",  "), 
                " \tRo:", table.concat(Ro, ", ")
            }, " | "))
        end
    end
    function gmtry(s, p)
        local desc = "Test:  gmatch ".. s .." -- ".. p
        local ri, ro = {}, {}
        for a, b, c, d, e, f in strung.gmatch(s, p) do
            ro[#ro + 1] = {a, b, c, d, e}
        end
        for a, b, c, d, e, f in string.gmatch(s, p) do
            ri[#ri + 1] = {a, b, c, d, e}
        end
        strung.assert(#ro == #ri, p, desc.."\nstring: \n"..ttstr(ri).."\n=/=/=/=/=/=/=/=/\nstrung:\n"..ttstr(ro))
        for i = 1, #ro do
        strung.assert(#ro[i] == #ri[i], p, desc.."\nstring: \n"..ttstr(ri).."\n=/=/=/=/=/=/=/=/\nstrung:\n"..ttstr(ro))
            for j = 1, #ri[i] do
                strung.assert(ri[i][j] == ro[i][j], p, desc.."\nstring: \n"..ttstr(ri).."\n=/=/=/=/=/=/=/=/\nstrung:\n"..ttstr(ro))
            end
        end
    end
    iter = function()end
end


--- Character classes and locales ---

local allchars do
    local acc = {}
    for i = 0, 255 do acc[i+1] = string.char(i) end
    allchars = table.concat(acc)
end

-- try("find", allchars, "a")
-- try("find", allchars, "%a+")
-- gmtry(allchars, "%a+")

for _, locale in ipairs{
    -- let this out for now, LJ character classes are not sensitive to os.setlocale()
    -- "fr_FR",
    "C"
} do
    -- print("LOCALE: ", strung.setlocale(locale))
    for c in ("acdlpsuwx"):gmatch"." do
        gmtry(allchars, "%"..c.."+")
        gmtry(allchars, "%"..c:upper().."+")
    end
end

--- .install() ---

local _f, _m, _gm, _gs, _ol = string.find, string.match, string.gmatch, string.gsub, os.locale

strung.install()
assert(
    string.find == strung.find
    and string.match == strung.match
    and string.gmatch == strung.gmatch
    --and string.gsub == strung.gsub
    and os.locale == strung.locale
    , "`strung.install()` failed.")
--restore the originals.
string.find, string.match, string.gmatch, string.gsub, os.locale = _f, _m, _gm, _gs, _ol

--- The tests (in reverse order of complexity)

--- %f ---
try("find", "a", "a?a")

try("find", "AAAAAA", "%f[%l]a")
try("find", "AAAAAA", "%f[%l]")
try("find", "aAaAb", "%f[%l]a", 2)
try("find", "aAaAb", "%f[%l]a")
try("find", "aAaAb", "%f[%l]a", 4)
try("find", "AaAb", "%f[%l]a")
try("find", "aAb", "%f[%l]b")
try("find", "aAb", "%f[%l]a")


--- negative indices ---

try("find", "fof", "f", -4)
try("find", "fof", "f", -3)
try("find", "fof", "f", -2)
try("find", "fof", "f", -1)

try("find", "fof", "[^o]", -4)
try("find", "fof", "[^o]", -3)
try("find", "fof", "[^o]", -2)
try("find", "fof", "[^o]", -1)

try("match", "fof", "f", -4)
try("match", "fof", "f", -3)
try("match", "fof", "f", -2)
try("match", "fof", "f", -1)

try("match", "fof", "[^o]", -4)
try("match", "fof", "[^o]", -3)
try("match", "fof", "[^o]", -2)
try("match", "fof", "[^o]", -1)

--- gmatch ---

gmtry('abcdabcdabcd', "((a)(b)c)()(d)")
-- try("find", 'abcdabcdabcd', "((a)(b)c)()(d)")
-- try("find", 'abcdabcdabcd', "(a)(b)c(d)")
gmtry('abcdabcdabcd', "(a)(b)c(d)")
gmtry('abcdabcdabcd', "(a)(b)(d)")
gmtry('abcdabcdabcd', "(a)(b)(d)")
gmtry('abcabcabc', "(a)(b)")
gmtry('abcabcabc', "(ab)")

iter(10)
--- bug fix ---

try("match", "faa:foo:", "(.+):(%l+)")
try("match", ":foo:", "(%l*)")
try("match", "faa:foo:", ":%l+")
try("match", "faa:foo:", ":(%l+)")
try("match", "faa:foo:", "(%l+)")
try("match", ":foo:", "(%l+)")
try("match", "foo", "%l+")
try("match", "foo", "foo")

--- anchored patterns ---

try("find", "wwS", "^wS", 2)
try("find", "wwS", "^wS")
try("find", "wwS", "^ww", 2)
try("find", "wwS", "^ww")

--- %b ---

try("find", "a(f()g(h(d d))[[][]]K)", "%b()%b[]", 3)
try("find", "a(f()g(h(d d))[[][]]K)", "%b()%b[]", 2)
try("find", "a(f()g(h(d d))[[][]]K)", "%b()%b[]")
try("find", "a(f()g(h(d d))K)", "%b()")
try("find", "a(f()g(h(d d))K", "%b()")
try("find", "foobarfoo", "(foo)(bar)%2%1")
try("find", "foobarbarfoo", "(foo)(bar)%2%1")
try("find", "foobarbar", "(foo)(bar)%2")
try("find", "foobarfoo", "(foo)(bar)%2")
try("find", "foobarfoo", "(foo)(bar)%1")
try("find", "foobarfoo", "(foo)bar%1")

--- Captures ---

try("find", "wwS", "((w*)S)")
try("find", "wwwwS", "((w*)%u)")
try("find", "wwS", "((%l)%u)")
try("find", "SSw", "((%u)%l)")
try("find", "wwS", "((%l*)%u)")
try("find", "wwS", "((%l-)%u)")

try("find", "wwS", "((w*)%u)")
try("find", "wwS", "((ww)%u)")
try("find", "wwS", "((%l*)S)")
try("find", "wwS", "((%l*))")


try("find", "wwSS", "()(%u+)()")

--- Character sets  ---

try("find", "wwwwwwS", "[^%u]*")
try("find", "wwwwwwS", "[^%u]")
try("find", "wwwwwwS", "(%l*)")
try("find", "wwSS", "(%u+)")

try("find", "wwS", "%l%u")
try("find", "wwS", "()%l%u")

try("find", "wwwwwwS", "[^%U]")
try("find", "wwwwwwS", "[%U]*")
try("find", "wwwwwwS", "[%U]+")

try("find", "wwwwwwS", "%l*")

try("find", "wwwwwwS", "[%U]")
try("find", "wwwwwwS", "[%u]")

try("find", "wwwwwwS", "[%u]*")
try("find", "wwwwwwS", "[^kfdS]*")

try("find", "wwS", "%l*()")
try("find", "wwS", "()%u+")

--- escape sequeces ---

try("find", "w(wSESDFB)SFwe)fwe", "%(.-%)")
try("find", "w(wSESDFB)SFwe)fwe", "%(.*%)")

--- Basic patterns ---

try("find", "wawSESDFB)SFweafwe", "a.-a")
try("find", "wawSESDFBaSFwe)fwe", "a.*a")

try("find", "a", ".")
try("find", "a6ruyfhjgjk9", ".+")



try("find", "wawSESDFBaSFwe)fwe", "a[A-Za-z]*a")

try("find", "qwwSYUGJHDwefwe", "%u+")
try("find", "wwSESDFBSFwefwe", "[A-Z]+")

try("find", "SYUGJHD", "%u+")
try("find", "SESDFBSF", "[A-Z]+")
try("find", "qwwSYUGJHD", "%u+")
try("find", "wwSESDFBSF", "[A-Z]+")

try("find", "S", "%u")
try("find", "S", "[A-Z]")

try("find", "ab", "a?b")
try("find", "b", "a?b")
try("find", "abbabbab", "a?ba?ba?ba?ba?b$")
try("find", "abbabbaba", "a?ba?ba?ba?ba?b$")

try("find", "aaaabaaaaabbaaaabb$", "a+bb$")
try("find", "aaaabaaaaabbaaaabb", "a+bb$")
try("find", "aaaaaaaabaaabaaaaabb", "a+bb")
try("find", "aaaaaaaabaaabaaaaabb", "a*bb")

try("find", "aaaaaaaabaaabaaaaab", "ba-bb")
try("find", "aaaaaaaabaaabaaaaabb", "ba-bb")

try("find", "aaa", "a+")
try("find", "aaaaaaaaaaaaaaaaaa", "a+")

try("find", "aaaaabaaaaabaaaaaaaaabb", "aabb")
try("find", "aaaaaaaaabbaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb", "aaaaaaaaaaaaabbb")

iter(100)

try("find", "baa", "aa")
try("find", "ba", "a")

try("find", "a", "aa")
try("find", "aa", "a")
try("find", "aa", "aa")
try("find", "a", "a")



if arg[1] ~= "bench" then print "ok" end