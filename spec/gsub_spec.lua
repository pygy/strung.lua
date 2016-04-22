require 'spec.helper'

describe('gsub', function ()

  local strung, gsub

  setup(function ()
    strung = require 'strung'
    gsub   = strung.gsub
  end)

  teardown(function ()
    strung, gsub = nil, nil
  end)

  it('basic tests', function ()
    assert.results('_+_+_', 2, gsub('_d_d_',   'd',    '+'))
    assert.results('_+_+_', 2, gsub('_da_da_', '(d)a', '+'))

    assert.results('_d_d_', 2, gsub('_d_d_', 'd', {}))
    assert.results('_9_9_', 2, gsub('_d_d_', 'd', {d = 9}))
    assert.results('_9_9_', 2, gsub('_d_d_', 'd', {d = '9'}))

    assert.results('_d_d_',  2, gsub('_d_d_',  'd',    function () end))
    assert.results('_do_d_', 2, gsub('_do_d_', 'd(.)', function (a, b) return b, a end))
  end)

  it('basic patterns', function ()
    assert.results('123IVXabcIVX123', 1, gsub('123abc123', 'abc', 'IVX%1IVX'))
    assert.results('()(..*) %1',      1, gsub('(..*) %1', '^(^?)', '%1()', 1))
    assert.results('(..*) %1()',      1, gsub('(..*) %1', '($?)$', '()%1', 1))
    assert.results(
      '(..*) %2', 1, gsub('(..*) %1', '%%([0-9])', function (s) return "%" .. (s+1) end)
    )
  end)

  it('should not retry to match anchored patterns following an initial success, issue #7', function ()
    assert.results('d:/dropbox/goluwa/...', 1, gsub('os:d:/dropbox/goluwa/...', '^(.-:)', ''))
    assert.results('d:/dropbox/goluwa/...', 1, gsub('os:d:/dropbox/goluwa/...', '^(.-:)', '', 2))
  end)
end)