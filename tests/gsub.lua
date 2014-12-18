--- gsub ---

local c = require"tests.common"
local try, gmtry, iter, allchars, dumpacc = c.try, c.gmtry, c.iter, c.allchars, c.dumpacc

try("gsub", "_d_d_", "d", "+")
try("gsub", "_da_da_", "(d)a", "+")

try("gsub", "_d_d_", "d", {})
try("gsub", "_d_d_", "d", {d = 9})
try("gsub", "_d_d_", "d", {d = "9"})

try("gsub", "_d_d_", "d", function()end)
try("gsub", "_do_d_", "(d)(.)", function(a,b) return b,a end)

