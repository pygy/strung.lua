require 'spec.helper'

describe('static', function ()

  local strung, find, match

  setup(function ()
    strung = require 'strung'
    find, match = strung.find, strung.match
  end)

  teardown(function ()
    strung, find, match = nil, nil, nil
  end)

  it('find with negative indexes', function ()
    assert.results(1, 1, find('fof', 'f', -4))
    assert.results(1, 1, find('fof', 'f', -3))
    assert.results(3, 3, find('fof', 'f', -2))
    assert.results(3, 3, find('fof', 'f', -1))
  end)

  it('find with negative indexes and plain search flag', function ()
    assert.results(1, 1, find('fof', 'f', -4, true))
    assert.results(1, 1, find('fof', 'f', -3, true))
    assert.results(3, 3, find('fof', 'f', -2, true))
    assert.results(3, 3, find('fof', 'f', -1, true))
  end)

  it('find with long string sequences', function ()
    assert.results(20, 23, find('aaaaabaaaaabaaaaaaaaabb', 'aabb'))
    assert.are.equals(
      find('aaaaaaaaabbaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb', 'aaaaaaaaaaaaabbb'), nil
    )
    assert.are.equals(
      find(('Long 1 -- aaaaaaaaabaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb'):rep(10000),
        'aaaaaaaaaaaaabbb'), nil
    )
    assert.are.equals(
      find(('Long 2 -- aaaaaaaaabaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb'):rep(10000),
        'aaaaaaaaaaaaaaaaaaaaaaaabbb'), nil
    )
    assert.are.equals(
      find(('Long 3 -- aaaaaaaaabaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb'):rep(10000),
        'aaaaaaaabbb'), nil
    )
    assert.are.equals(
      find(('Long 4 -- aaaaaaaaabaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb'):rep(10000),
        'c'), nil
    )
  end)

  it('find short sequences', function ()
    assert.results(2, 3, find('baa', 'aa'))
    assert.results(2, 2, find('ba', 'a'))
    assert.results(1, 1, find('aa', 'a'))
    assert.results(1, 2, find('aa', 'aa'))
    assert.results(1, 1, find('a', 'a'))
    assert.are.equals(find('a', 'aa'), nil)
  end)

  it('find short and average sequences with nil index and plain search', function ()    
    assert.are.equals(find('a', 'aa', nil, true), nil)
    assert.are.equals(
      find('aaaaaaaaabbaaaaaaaabbaaaaaaaaaaaabbaaaaaaaaaaabbaaaaaaaaaaaabb', 'aaaaaaaaaaaaabbb',
        nil, true), nil
    )
    assert.results(20, 23, find('aaaaabaaaaabaaaaaaaaabb', 'aabb', nil, true))
    assert.results(2, 3, find('baa', 'aa', nil, true))
    assert.results(2, 2, find('ba', 'a', nil, true))
    assert.results(1, 1, find('aa', 'a', nil, true))
    assert.results(1, 2, find('aa', 'aa', nil, true))
    assert.results(1, 1, find('a', 'a', nil, true))
  end)

  it('match with negative indexes', function ()
    assert.are.equals(match('fof', 'f', -4), 'f')
    assert.are.equals(match('fof', 'f', -3), 'f')
    assert.are.equals(match('fof', 'f', -2), 'f')
    assert.are.equals(match('fof', 'f', -1), 'f')
  end)
end)