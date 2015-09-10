require 'spec.helper'

describe('pattern matching', function ()

  local string, strung, table, char, find, gfind, gmatch, gsub, len, match, rep, sub, upper, concat,
    insert, remove, f

  setup(function ()
    string, strung, table = require 'string', require 'strung', require 'table'

    char, find, gfind, gmatch, gsub, len, match, rep, sub, upper, concat, insert, remove =
      string.char, strung.find, strung.gfind, strung.gmatch, strung.gsub, string.len, strung.match,
      string.rep, string.sub, string.upper, table.concat, table.insert, table.remove

    f = function (s, p)
      local i, e = find(s, p)
      if i then
        return sub(s, i, e)
      end
    end
  end)

  teardown(function ()
    string, strung, table, char, find, gfind, gmatch, gsub, len, match, rep, sub, upper, concat,
      insert, remove, f =
      nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil
  end)

  it('empty pattern', function ()
    assert.results(1, 0, find('', ''))
    assert.results(1, 0, find('alo', ''))
  end)

  it('first position', function ()
    assert.results(1, 1, find('a\0o a\0o a\0o', 'a', 1))
    assert.are.equals(find('alo123alo', '^12'), nil)
  end)

  it('starts in the middle', function ()
    assert.results(5, 7,  find('a\0o a\0o a\0o', 'a\0o', 2))
    assert.results(9, 11, find('a\0o a\0o a\0o', 'a\0o', 9))
    assert.results(4, 5,  find('alo123alo', '12'))
  end)

  it('last position', function ()
    assert.results(11, 11, find('a\0a\0a\0a\0\0ab', 'b'))
  end)

  it('check ending', function ()
    assert.is_nil(find('a\0a\0a\0a\0\0ab', 'b\0'))
    assert.is_nil(find('', '\0'))
  end)

  it('check for negative indices', function ()
    assert.are.equal(match('fof', '[^o]', -4), 'f')
    assert.are.equal(match('fof', '[^o]', -3), 'f')
    assert.are.equal(match('fof', '[^o]', -2), 'f')
    assert.are.equal(match('fof', '[^o]', -1), 'f')
    assert.are.equal(match('fof', 'f', -4),    'f')
    assert.are.equal(match('fof', 'f', -3),    'f')
    assert.are.equal(match('fof', 'f', -2),    'f')
    assert.are.equal(match('fof', 'f', -1),    'f')
  end)

  it('check for lower case', function ()
    assert.results('faa', 'foo', match('faa:foo:', '(.+):(%l+)'))
    assert.are.equal(match(':foo:', '(%l*)'),     '')
    assert.are.equal(match('faa:foo:', ':%l+'),   ':foo')
    assert.are.equal(match('faa:foo:', ':(%l+)'), 'foo')
    assert.are.equal(match('faa:foo:', '(%l+)'),  'faa')
    assert.are.equal(match(':foo:', '(%l+)'),     'foo')
    assert.are.equal(match('foo', '%l+'),         'foo')
    assert.are.equal(match('foo', 'foo'),         'foo')
  end)

  it('subtract greedy', function ()
    assert.are.equals(f('aloALO', '%l*'),               'alo')
    assert.are.equals(f('aLo_ALO', '%a*'),              'aLo')
    assert.are.equals(f('aaab', 'a*'),                  'aaa')
    assert.are.equals(f('aaa', '^.*$'),                 'aaa')
    assert.are.equals(f('aaa', 'b*'),                   '')
    assert.are.equals(f('aaa', 'ab*a'),                 'aa')
    assert.are.equals(f('aba', 'ab*a'),                 'aba')
    assert.are.equals(f('aaab', 'a+'),                  'aaa')
    assert.are.equals(f('aaa', '^.+$'),                 'aaa')
    assert.are.equals(f('aba', 'ab+a'),                 'aba')
    assert.are.equals(f('', 'b*'),                      '')
    assert.are.equals(f('aabaaabaaabaaaba', 'b.*b'),    'baaabaaabaaab')
    assert.are.equals(f(' \n isto ? assim', '%S%S*'),   'isto')
    assert.are.equals(f(' \n isto ? assim', '%S*$'),    'assim')
    assert.are.equals(f(' \n isto ? assim', '[a-z]*$'), 'assim')
    assert.are.equals(f('0alo alo', '%x*'),             '0a')
    assert.are.equals(f('alo alo', '%C+'),              'alo alo')

    assert.is_nil(f('aaa', 'bb*'))
    assert.is_nil(f('aaa', 'b+'))
    assert.is_nil(f('aaa', 'ab+a'))
  end)

  it('subtract non-greedy and optional', function ()
    assert.are.equals(f('a$a', '.$'),                       'a')
    assert.are.equals(f('a$a', '.%$'),                      'a$')
    assert.are.equals(f('a$a', '.$.'),                      'a$a')
    assert.are.equals(f('a$a', '$'),                        '')
    assert.are.equals(f('aaab', 'a-'),                      '')
    assert.are.equals(f('aaa', '^.-$'),                     'aaa')
    assert.are.equals(f('aabaaabaaabaaaba', 'b.-b'),        'baaab')
    assert.are.equals(f('alo xo', '.o$'),                   'xo')
    assert.are.equals(f('um caracter ? extra', '[^%sa-z]'), '?')
    assert.are.equals(f('', 'a?'),                          '')
    assert.are.equals(f('?', '??'),                         '?')
    assert.are.equals(f('?bl', '??b?l?'),                   '?bl')
    assert.are.equals(f('  ?bl', '??b?l?'),                 '')
    assert.are.equals(f('aa', '^aa?a?a'),                   'aa')
    assert.are.equals(f(']]]?b', '[^]]'),                   '?')

    assert.is_nil(f('a$a', '$$'))
    assert.is_nil(f('a$b', 'a$'))
  end)

  it('capture string', function ()

    local function f1 (s, p)
      p = gsub(p, '%%([0-9])', function (s) return '%' .. (s+1) end)
      p = gsub(p, '^(^?)', '%1()', 1)
      p = gsub(p, '($?)$', '()%1', 1)
      local t = {match(s, p)}
      return sub(s, t[1], t[#t] - 1)
    end

    assert.are.equals(f1('alo alx 123 b\0o b\0o', '(..*) %1'),        'b\0o b\0o')
    assert.are.equals(f1('axz123= 4= 4 34', '(.+)=(.*)=%2 %1'),       '3= 4= 4 3')
    assert.are.equals(f1('=======', '^(=*)=%1$'),                     '=======')
    assert.are.equals(match('alo xyzK', '(%w+)K'),                    'xyz')
    assert.are.equals(match('alo xyzK', '(%w+)K'),                    'xyz')
    assert.are.equals(match('254 K', '(%d*)K'),                       '')
    assert.are.equals(match('alo ', '(%w*)$'),                        '')
    assert.are.equals(gsub('  alo alo  ', '^%s*(.-)%s*$', '%1'),      'alo alo')
    assert.are.equals(gsub('alo alo', '()[al]', '%1'),                '12o 56o')
    assert.are.equals(gsub('abc=xyz', '(%w*)(%p)(%w+)', '%3%2%1-%0'), 'xyz=abc-abc=xyz')

    assert.is_nil(match('alo ', '(%w+)$'))
    assert.is_nil(match('==========', '^([=]*)=%1$'))

    assert.results('?lo alo', '?l', '?', 'alo', match('?lo alo', '^(((.).).* (%w*))$'))
    assert.results('0123456789', '', 11,        match('0123456789', '(.+(.?)())'))
    assert.results('a@b@?d', 2,                 gsub('ab?d', '(.)', '%0@', 2))
  end)

  it('subtract enclosed sequences', function ()
    local res = {''}

    for i = 0, 255 do
      insert(res, char(i))
    end

    local abc = concat(res)

    local function strset (p)
      res = {''}
      gsub(abc, p, function (c) insert(res, c) end)
      return concat(res)
    end

    assert.are.equals(strset('[a-z]'),            'abcdefghijklmnopqrstuvwxyz')
    assert.are.equals(strset('[a-z%d]'),          strset('[%da-uu-z]'))
    assert.are.equals(strset('[a-]'),             '-a')
    assert.are.equals(strset('[^%W]'),            strset('[%w]'))
    assert.are.equals(strset('[]%%]'),            '%]')
    assert.are.equals(strset('[a%-z]'),           '-az')
    assert.are.equals(strset('[%^%[%-a%]%-b]'),   '-[]^ab')
    assert.are.equals(len(strset('[\110-\120]')), 11)
    assert.are.equals(len(strset('[\200-\210]')), 11)
    assert.are.equals(strset('[\1-\255]'),        strset('%Z'))
    assert.are.equals(strset('[\1-\255%z]'),      strset('.'))
  end)

  it('function modifiers', function ()

    local scope, t, s = {gsub=gsub, upper=upper}, {}, ''

    local function setglobal (n, v)
      scope[n] = v
    end

    local function dostring (s)
      return (load or function (block, _, __, env)
        if env ~= nil then
          return setfenv(loadstring(block), env)
        end
        return loadstring(block)
      end)(s, nil, nil, scope)() or ''
    end

    local function isbalanced (s)
      return find(gsub(s, '%b()', ''), '[()]') == nil
    end

    assert.are.equals(
      gsub('alo $a=1$ novamente $return a$', '$([^$]*)%$', dostring), 'alo  novamente 1'
    )
    assert.are.equals(
      gsub("$x=gsub('alo', '.', upper)$ assim vai para $return x$", '$([^$]*)%$', dostring),
      ' assim vai para ALO'
    )
    gsub('a=roberto,roberto=a', '(%w+)=(%w%w*)', setglobal)
    assert.are.equals(scope.a,       'roberto')
    assert.are.equals(scope.roberto, 'a')

    t = {'apple', 'orange', 'lime', n=0}
    assert.are.equals(
      gsub('x and x and x', 'x', function () t.n = t.n + 1 return t[t.n] end),
      'apple and orange and lime'
    )
    
    t, s    = {}, 'a alo jose  joao'
    local r = strung.gsub(s, '()(%w+)()', function (a, w, b)
      assert.are.equals(len(w), b - a)
      t[a] = b - a;
    end)
    assert.are.same(t, {[1]=1, [3]=3, [7]=4, [13]=4})
    assert.are.equals(s, r)

    t = {n=0}
    gsub('first second word', '%w%w*', function (w)
      t.n = t.n + 1 t[t.n] = w
    end)
    assert.are.same(t, {'first', 'second', 'word', n=3})

    t = {n=0}
    assert.are.equals(
      gsub('first second word', '%w+', function (w) t.n = t.n + 1 t[t.n] = w end, 2),
      'first second word'
    )
    assert.are.same(t, {'first', 'second', n = 2})
    assert.truthy(isbalanced("(9 ((8))(\0) 7) \0\0 a b ()(c)() a"))
    assert.not_truthy(isbalanced("(9 ((8) 7) a b (\0 c) a"))

    --[[assert.are.equals(
      gsub(
        'trocar tudo em |teste|b| ? |beleza|al|", "|([^|]*)|([^|]*)|',
        function (a, b) return gsub(a, '.', b) end
      ), 'trocar tudo em bbbbb ? alalalalalal'
    )--]]

    assert.are.equals(
      gsub(
        'trocar tudo em |teste|b| ? |beleza|al|', '|([^|]*)|([^|]*)|',
        function (a, b) return gsub(a, '.', b) end
      ), 'trocar tudo em bbbbb ? alalalalalal'
    )
    assert.are.equals(
      gsub('um (dois) tres (quatro)', '(%(%w+%))', upper), 'um (DOIS) tres (QUATRO)'
    )
  end)

  it('broken patterns', function ()
    assert.is.falsy(pcall(gsub, 'alo', '(.',   print))
    assert.is.falsy(pcall(gsub, 'alo', '.)',   print))
    assert.is.falsy(pcall(gsub, 'alo', '(.',   {}))
    assert.is.falsy(pcall(gsub, 'alo', '(.)',  '%2'))
    assert.is.falsy(pcall(gsub, 'alo', '(%1)', 'a'))
    assert.is.falsy(pcall(gsub, 'alo', '(%0)', 'a'))
  end)

  it('big strings', function ()
    local a = rep('a', 300000)

    assert.results(1, 300000, find(a, '^a*.?$'))
    assert.results(1, 300000, find(a, '^a-.?$'))
    assert.is_nil(find(a, '^a*.?b$'))
  end)

  it('deep nests', function ()
    local x = rep('012345', 10)
    local function rev (s)
      return gsub(s, '(.)(.+)', function (c, s1) return rev(s1)..c end)
    end

    assert.is.equals(rev(rev(x)), x)
  end)

  it('gsub with tables', function ()
    local t = {}
    setmetatable(t, {
      __index = function (t, s)
        return upper(s)
      end
    })

    assert.are.equals(gsub('alo alo', '.', {}),                         'alo alo')
    assert.are.equals(gsub('alo alo', '(.)', {a='AA', l=''}),           'AAo AAo')
    assert.are.equals(gsub('alo alo', '(.).', {a='AA', l='K'}),         'AAo AAo')
    assert.are.equals(gsub('alo alo', '((.)(.?))', {al='AA', o=false}), 'AAo AAo')
    assert.are.equals(gsub('alo alo', '().', {2,5,6}),                  '256 alo')
    assert.are.equals(gsub('a alo b hi', '%w%w+', t),                   'a ALO b HI')
  end)

  it('gmatch', function ()
    local a, t = 0, {n=0}
    assert.are.equals(gfind, gmatch)
    
    for i in gmatch('abcde', '()') do
      assert.are.equals(i, a + 1) a = i
    end
    assert.are.equals(a, 6)

    for w in gmatch('first second word', '%w+') do
      t.n    = t.n + 1
      t[t.n] = w
    end
    assert.are.same(t, {'first', 'second', 'word', n = 3})

    t = {3, 6, 9}
    for i in gmatch('xuxx uu ppar r', '()(.)%2') do
      assert.are.equals(remove(t, 1), i)
    end
    assert.are.equals(#t, 0)
    for i, j in gmatch('13 14 10 = 11, 15= 16, 22=23', '(%d+)%s*=%s*(%d+)') do
      t[i] = j
    end
    
    a = 0
    for k,v in pairs(t) do
      assert.are.equals(k + 1, v + 0)
      a = a + 1
    end
    assert.are.equals(a, 3)

    t = {}
    for i, j, k, l, m in gmatch('abcdabcdabcd', '((a)(b)c)()(d)') do
      insert(t, {i, j, k, l, m})
    end
    assert.are.same(
      t, {{'abc', 'a', 'b', 4, 'd'}, {'abc', 'a', 'b', 8, 'd'}, {'abc', 'a', 'b', 12,  'd'}}
    )

    t = {}
    for a, b, d in gmatch('abcdabcdabcd', '(a)(b)c(d)') do
      insert(t, {a, b, d})
    end
    assert.are.same(t, {{'a', 'b', 'd'}, {'a', 'b', 'd'}, {'a', 'b', 'd'}})

    t = {}
    for a, b, d in gmatch('abcdabcdabcd', '(a)(b)(d)') do
      insert(t, {a, b, d})
    end
    assert.are.same(t, {})

    t = {}
    for a, b in gmatch('abcabcabc', '(a)(b)') do
      insert(t, {a, b})
    end
    assert.are.same(t, {{'a', 'b'}, {'a', 'b'}, {'a', 'b'}})

    t = {}
    for ab in gmatch('abcabcabc', '(ab)') do
      insert(t, ab)
    end
    assert.are.same(t, {'ab', 'ab', 'ab'})
  end)

  it('other tests', function ()
    assert.are.equals(find("(?lo)", "%(?"),                    1)
    assert.are.equals(gsub('?lo ?lo', '?', 'x'),               'xlo xlo')
    assert.are.equals(gsub('alo ?lo  ', ' +$', ''),            'alo ?lo')
    assert.are.equals(gsub('alo  alo  \n 123\n ', '%s+', ' '), 'alo alo 123 ')
    assert.are.equals(gsub('abc', '%w', '%1%0'),               'aabbcc')
    assert.are.equals(gsub('abc', '%w+', '%0%1'),              'abcabc')
    assert.are.equals(gsub('???', '$', '\0??'),                '???\0??')
    assert.are.equals(gsub('', '^', 'r'),                      'r')
    assert.are.equals(gsub('', '$', 'r'),                      'r')
  end)

  it('frontiers', function ()
    assert.are.equals(gsub('aaa aa a aaa a', '%f[%w]a', 'x'), 'xaa xa x xaa x')
    assert.are.equals(gsub('[[]] [][] [[[[', '%f[[].', 'x'),  'x[]] x]x] x[[[')
    assert.are.equals(gsub('01abc45de3', '%f[%d]', '.'),      '.01abc.45de.3')
    assert.are.equals(gsub('01abc45 de3x', '%f[%D]%w', '.'),  '01.bc45 de3.')
    assert.are.equals(gsub('function', '%f[\1-\255]%w', '.'), '.unction')
    assert.are.equals(gsub('function', '%f[^\1-\255]', '.'),  'function.')

    assert.results(2, 5, find(' alo aalo allo', '%f[%S].-%f[%s].-%f[%S]'))
    assert.are.equals(match(' alo aalo allo', '%f[%S](.-%f[%s].-%f[%S])'), 'alo ')

    local t = {1, 5, 9, 14, 17,}
    for k in strung.gmatch('alo alo th02 is 1hat', '()%f[%w%d]') do
      assert.are.equals(remove(t, 1), k)
    end
    assert.are.equals(#t, 0)
  end)

end)