require 'spec.helper'

describe('patterns', function ()

  local find

  setup(function ()
    find = require('strung').find
  end)

  teardown(function ()
    find = nil
  end)

  it('zero', function ()
    assert.is.truthy(pcall(find, '\0', '[\0]'))
    assert.results(1, 1, find('\0', '\0'))
    assert.results(1, 1, find('\0', '%z'))
    assert.results(1, 1, find('\0', '[%z]'))
    assert.is_nil(find('', '[%z]'))
  end)

  it('frontiers (%f)', function ()
    assert.is_nil(find('AAAAAA', '%f[%l]a'))
    assert.is_nil(find('AAAAAA', '%f[%l]'))
    assert.results(3, 3, find('aAaAb', '%f[%l]a', 2))
    assert.results(1, 1, find('aAaAb', '%f[%l]a'))
    assert.is_nil(find('aAaAb', '%f[%l]a', 4))
    assert.results(2, 2, find('AaAb', '%f[%l]a'))
    assert.results(3, 3, find('aAb', '%f[%l]b'))
    assert.results(1, 1, find('aAb', '%f[%l]a'))
  end)

  it('negative indices', function ()
    assert.results(1, 1, find('fof', '[^o]', -4))
    assert.results(1, 1, find('fof', '[^o]', -3))
    assert.results(3, 3, find('fof', '[^o]', -2))
    assert.results(3, 3, find('fof', '[^o]', -1))
  end)

  it('anchored patterns', function ()
    assert.results(2, 3, find('wwS', '^wS', 2))
    assert.results(1, 2, find('wwS', '^ww'))

    assert.is_nil(find('wwS', '^wS'))
    assert.is_nil(find('wwS', '^ww', 2))
  end)

  it('substring, "between" pattern', function ()
    assert.results(7, 20, find('a(f()g(h(d d))[[][]]K)', '%b()%b[]', 3))
    assert.results(7, 20, find('a(f()g(h(d d))[[][]]K)', '%b()%b[]', 2))
    assert.results(7, 20, find('a(f()g(h(d d))[[][]]K)', '%b()%b[]'))
    assert.results(2, 16, find('a(f()g(h(d d))K)', '%b()'))
    assert.results(4, 5,  find('a(f()g(h(d d))K', '%b()'))
  end)

  it('references', function ()
    assert.is_nil(find('foobarfoo', '(foo)(bar)%2%1'))
    assert.results(1, 12, 'foo', 'bar', find('foobarbarfoo', '(foo)(bar)%2%1'))
    assert.results(1, 9, 'foo', 'bar', find('foobarbar', '(foo)(bar)%2'))
    assert.is_nil(find('foobarfoo', '(foo)(bar)%2'))
    assert.results(1, 9, 'foo', 'bar', find('foobarfoo', '(foo)(bar)%1'))
    assert.results(1, 9, 'foo', find('foobarfoo', '(foo)bar%1'))
  end)

  it('captures', function ()
    assert.results(1, 3, 'wwS', 'ww', find('wwS', '((w*)S)'))
    assert.results(1, 5, 'wwwwS', 'wwww', find('wwwwS', '((w*)%u)'))
    assert.results(2, 3, 'wS', 'w', find('wwS', '((%l)%u)'))
    assert.results(2, 3, 'Sw', 'S', find('SSw', '((%u)%l)'))
    assert.results(1, 3, 'wwS', 'ww', find('wwS', '((%l*)%u)'))
    assert.results(1, 3, 'wwS', 'ww', find('wwS', '((%l-)%u)'))
    assert.results(1, 3, 'wwS', 'ww', find('wwS', '((w*)%u)'))
    assert.results(1, 3, 'wwS', 'ww', find('wwS', '((ww)%u)'))
    assert.results(1, 3, 'wwS', 'ww', find('wwS', '((%l*)S)'))
    assert.results(1, 2, 'ww', 'ww', find('wwS', '((%l*))'))
    assert.results(3, 4, 3, 'SS', 5, find('wwSS', '()(%u+)()'))
  end)

  it('character sets', function ()
    assert.results(1, 5, find('wwwww]wS', '[^%u%]]*'))
    assert.results(1, 4, find('%]%]', '[%%%]]+'))
    assert.results(1, 5, find('%]]]]%]', '[%%]]+'))

    assert.results(1, 6, find('wwwwwwS', '[^%u]*'))
    assert.results(1, 1, find('wwwwwwS', '[^%u]'))
    assert.results(1, 6, 'wwwwww', find('wwwwwwS', '(%l*)'))
    assert.results(3, 4, 'SS', find('wwSS', '(%u+)'))

    assert.results(2, 3, find('wwS', '%l%u'))
    assert.results(2, 3, 2, find('wwS', '()%l%u'))

    assert.results(7, 7, find('wwwwwwS', '[^%U]'))
    assert.results(1, 6, find('wwwwwwS', '[%U]*'))
    assert.results(1, 6, find('wwwwwwS', '[%U]+'))

    assert.results(1, 6, find('wwwwwwS', '%l*'))

    assert.results(1, 1, find('wwwwwwS', '[%U]'))
    assert.results(7, 7, find('wwwwwwS', '[%u]'))

    assert.results(1, 0, find('wwwwwwS', '[%u]*'))
    assert.results(1, 6, find('wwwwwwS', '[^kfdS]*'))

    assert.results(1, 2, 3, find('wwS', '%l*()'))
    assert.results(3, 3, 3, find('wwS', '()%u+'))
  end)

  it('escape sequences', function ()
    assert.results(2, 10, find('w(wSESDFB)SFwe)fwe', '%(.-%)'))
    assert.results(2, 15, find('w(wSESDFB)SFwe)fwe', '%(.*%)'))
  end)

  it('basic patterns', function ()
    assert.results(2, 15, find('wawSESDFB)SFweafwe', 'a.-a'))
    assert.results(2, 10, find('wawSESDFBaSFwe)fwe', 'a.*a'))

    assert.results(1, 1,  find('a', '.'))
    assert.results(1, 12, find('a6ruyfhjgjk9', '.+'))

    assert.results(2, 10, find('wawSESDFBaSFwe)fwe', 'a[A-Za-z]*a'))

    assert.results(4, 10, find('qwwSYUGJHDwefwe', '%u+'))
    assert.results(3, 10, find('wwSESDFBSFwefwe', '[A-Z]+'))

    assert.results(1, 7,  find('SYUGJHD', '%u+'))
    assert.results(1, 8,  find('SESDFBSF', '[A-Z]+'))
    assert.results(4, 10, find('qwwSYUGJHD', '%u+'))
    assert.results(3, 10, find('wwSESDFBSF', '[A-Z]+'))

    assert.results(1, 1, find('S', '%u'))
    assert.results(1, 1, find('S', '[A-Z]'))

    assert.results(1, 1, find('a', 'a?a'))
    assert.results(1, 2, find('ab', 'a?b'))
    assert.results(1, 1, find('b', 'a?b'))
    assert.results(3, 5, find('abbab', 'a?ba?b$'))
    assert.results(6, 8, find('abbabbab', 'a?ba?b$'))
    assert.results(1, 8, find('abbabbab', 'a?ba?ba?ba?ba?b$'))

    assert.is_nil(find('abbabbaba', 'a?ba?ba?ba?ba?b$'))
    assert.is_nil(find('aaaabaaaaabbaaaabb$', 'a+bb$'))

    assert.results(13, 18, find('aaaabaaaaabbaaaabb', 'a*bb$'))
    assert.results(14, 20, find('aaaaaaaabaaabaaaaabb', 'a+bb'))
    assert.results(14, 20, find('aaaaaaaabaaabaaaaabb', 'a*bb'))

    assert.is_nil(find('aaaaaaaabaaabaaaaab', 'ba-bb'))

    assert.results(13, 20, find('aaaaaaaabaaabaaaaabb', 'ba-bb'))
    assert.results(1, 3,   find('aaa', 'a+'))
    assert.results(1, 18,  find('aaaaaaaaaaaaaaaaaa', 'a+'))
  end)

end)