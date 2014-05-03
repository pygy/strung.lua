local c = require"tests.common"
local try, gmtry, iter, allchars = c.try, c.gmtry, c.iter, c.allchars

try("find", allchars, "a")

try("find", "fof", "f", -4)
try("find", "fof", "f", -3)
try("find", "fof", "f", -2)
try("find", "fof", "f", -1)

try("match", "fof", "f", -4)
try("match", "fof", "f", -3)
try("match", "fof", "f", -2)
try("match", "fof", "f", -1)

-- iter(0.0001)
-- try("find", ("aaaaaaaaabaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb"):rep(10000), "aaaaaaaaaaaaabbb")

-- s = {}
-- for i = 1, 610000 do
--     s[#s+1] = string.char(math.random(255))
-- end
-- s = table.concat(s)
-- collectgarbage()
-- try("find", s, "aaaaaaaaaaaaabbb")

try("find", "aaaaabaaaaabaaaaaaaaabb", "aabb")
try("find", "aaaaaaaaabbaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb", "aaaaaaaaaaaaabbb")

iter(10)

try("find", "baa", "aa")
try("find", "ba", "a")

try("find", "a", "aa")
try("find", "aa", "a")
try("find", "aa", "aa")
try("find", "a", "a")

try("find", "aaaaabaaaaabaaaaaaaaabb", "aabb", nil, true)
try("find", "aaaaaaaaabbaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb", "aaaaaaaaaaaaabbb", nil, true)

iter(10)

try("find", "baa", "aa", nil, true)
try("find", "ba", "a", nil, true)

try("find", "a", "aa", nil, true)
try("find", "aa", "a", nil, true)
try("find", "aa", "aa", nil, true)
try("find", "a", "a", nil, true)

if not bench then print"ok" end