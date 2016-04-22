-- a simple busted tests helper

local say, util, assert, math, string, table, pretty =
  require 'say', require 'luassert.util', require 'luassert.assert', require 'math',
  require 'string', require 'table', require 'pl.pretty'

-- 
local format, gsub, remove, insert, concat, deep_cmp, write, mod =
  string.format, string.gsub, util.tremove, util.tinsert, table.concat, util.deepcompare,
  pretty.write, math.mod and math.mod or load('return function (a, b) return a % b end')()

string, table, util, math, pretty = nil, nil, nil, nil, nil

-- format result
--
-- @any    result
-- @return string

local function fmt_result (result)
  local tipo = type(result)
  if tipo == 'string' then
    return format('  (%s) "%s"', tipo, gsub(result, '"', '\\"'))
  end
  return format('  (%s) %s', tipo, (tipo == 'table' and write(result, '  ') or tostring(result)))
end

-- format & concatenate table
--
-- @table  tb
-- @string append
-- @return string

local function fmt_concat (tb, append)
  local result = {}
  for i = 1, #tb do
    insert(result, fmt_result(tb[i]))
  end
  return concat(result, append)
end

-- adjust arguments
--
-- @table  args
-- @number half
-- @return boolean|table

local function adjust (args, half)
  args[1] = {fmt_result(args[1])}
  for i = 2, half do
    insert(args[1], fmt_result(remove(args, 2)))
  end

  args[2] = {fmt_result(args[2])}
  for i = 2, half do
    insert(args[2], fmt_result(remove(args, 3))) 
  end

  args.nofmt, args[1], args[2] = {true, true}, concat(args[2], ', '), concat(args[1], ', ')
  return false
end

-- assert multiple results
--
-- @table  state
-- @table  args
-- @return boolean

local function results (state, args)
  assert(mod(args.n, 2) == 0, say('assertion.results.oddargs', {args.n, fmt_concat(args)}), 1)

  local half, t1, t2 = args.n / 2, '', ''

  for i = 1, half do
    t1, t2 = type(args[i]), type(args[i + half])
    if t1 ~= t2 then
      return adjust(args, half)
    else
      if t1 == 'table' then
        if not deep_cmp(args[i], args[i + half], true) then
          return adjust(args, half)
        end
      else
        if args[i] ~= args[i + half] then
          return adjust(args, half)
        end
      end
    end
  end

  return not adjust(args, half)
end

--
say:set('assertion.results.oddargs',  'Expect even number of arguments but got: %d, arguments\n%s')
say:set('assertion.results.positive', 'Expect results to be equal.\nPassed in:\n%s\nExpected:\n%s')
say:set(
  'assertion.results.negative',
  'Expect results to not be equal.\nPassed in:\n%s\nDid not expected:\n%s'
)

--
assert:register(
  'assertion', 'results', results, 'assertion.results.positive', 'assertion.results.negative'
)