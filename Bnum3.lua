--!strict
--!optimize 2

type BN = {man: number, exp: number}
type HN = {man: number, layer: number, exp: number}

local one: BN = {man =1, exp = 0}
local zero: BN = {man=0,exp=0}
local inf: BN = {man =1, exp = math.huge}
local neginf: BN = {man = -1, exp = math.huge}
local nan: BN = {man = 1, exp = 0/0}

--[[
able to compute man >= 10 then converts down to 1 then convert to exp
so man = 150, exp = 0
computes as 
{man = 1.5, exp = 2}
]]
function normalize(man: number, exp: number): BN
	if man == 0 or man ~= man then
		if man ~= man or exp ~= exp then
			return nan
		end
		return zero
	end
	local sign = 1
	local logMan = math.floor(math.log10(math.abs(man)))
	man /= 10^logMan
	exp += logMan
	if man > 0 and man < 1 then
		man *= 10
		exp -= 1
	end
	man = man * sign
	if exp >= math.huge then return inf elseif exp <= -math.huge then return neginf end
	return {man = man, exp = exp}
end

local Bn = {}
--[[
creates a new BN as in {man = 1, exp = 3} ex. 1000 but with normalize it converts it. so it doesnt do {man = 0.98, exp = 3} but its {man = 9.8, exp = 2}

and also able to compute exp as decimal to convert back to man as ex 1.25 from exp 0.1
]]
function Bn.new(man: number, exp: number): BN
	return normalize(man, exp)
end

-- converts lets say 15 to 1.5e1 so 100 is 1e2 1000 is 1e3 and so on
function Bn.fromNumber(val: number): BN
	if val == 0 then return zero end
	if val ~= val then return nan end
	local exp = math.floor(math.log10(math.abs(val)))
	local man = val / 10^exp
	return Bn.new(man ,exp)
end

-- converts BN back to number so {man = 1.5, exp= 2} is 150
function Bn.toNumber(val: BN): number
	if val.exp >= 308 then return math.huge end
	local man, exp = val.man, val.exp
	local result = man * 10^exp
	local scaled = math.floor(result * 100 + 0.001)/ 100
	return scaled
end

-- converts BN to string to store into a StringValue, base toHyper is 308 change it to what u want like 1000
function Bn.toStr(val: BN, toHyper: number?): string
	toHyper = toHyper or 308
	local man: number, exp: number = val.man, val.exp
	if exp >= math.huge then return 'Inf' end
	if exp <= -math.huge then return '-Inf' end
	if exp ~= exp then return 'NaN' end
	if exp >= toHyper then
		local fromNum = Bn.fromNumber(exp)
		return man .. 'e' .. Bn.toStr(fromNum, toHyper)
	end
	return man .. 'e' .. exp
end

-- helps convert toStr back to BN like '1e3' back to {man = 1, exp = 3} or 1e3.1e2 back to {man = 1.5, exp = 310}
function Bn.fromString(val: string): BN
	local exponent = val:find('e')
	if not exponent then
		local num = tonumber(val)
		if not num then return nan end
		return Bn.fromNumber(num)
	end
	local manStr, expStr = val:match('^(.-)e(.+)$')
	local man = tonumber(manStr)
	if not man then return nan end
	local str: string = expStr:: string
	local exp: number
	if str:find('e') then
		local expBN = Bn.fromString(str)
		exp = expBN.man * 10^ expBN.exp
	else
		exp = tonumber(str):: number
		if not exp then return nan end
	end
	return {man = man, exp = exp}
end

--[[
helper function to help convert {1, 3} to BN

or {1} just for {man= 1, exp = 0} or {1000} for {man = 1, exp = 3}

auto corrects {1, 2, 3} to convert to {man = 1, exp = 2}
]]
function Bn.fromTable(val: {number}): BN
	assert(typeof(val) == 'table', 'Must have {1, 2} or {1} not {1, 2, ...}')
	if #val >= 3 then 
		warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
		warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bn.toStr({man = val[1], exp = val[2]}), 'to:', Bn.new(val[1], val[2]))
		return Bn.new(val[1], val[2])
	end
	if #val == 2 then
		local man = val[1]
		local exp = val[2]
		return Bn.new(man, exp)
	elseif #val == 1 then
		return Bn.fromNumber(val[1])
	end
	warn('Failed to convert fromTable empty or cant do that')
	return zero
end

-- able to convert number as in 1000 to {man = 1, exp = 3}, '1e3' to {man = 1, exp = 3} '1e3.2e2' to {man = 1.5, exp = 310}, {1} to {man = 1, exp = 0} as in fromNumber, {1, 2} converts to {man = 1, exp = 2}, {1,235, 23} converts to {man = 1, exp = 235} and doesnt allow more then 2 in a table
function Bn.convert(val: any): BN
	if typeof(val) == 'number' then
		return Bn.fromNumber(val)
	elseif typeof(val) == 'string' then
		return Bn.fromString(val)
	elseif typeof(val) == 'table' then
		if val.man ~= nil and val.exp ~= nil then
			return Bn.new(val.man:: number, val.exp:: number)
		end
		return Bn.fromTable(val)
	end
	warn(`Failed to convert to BN autocorrect to {'zero'}`)
	return zero
end

-- acts like 1+1 but for BN like '1e0' + '1e0' is {man = 2, exp = 0}, '1e1' + '1e0' is {man = 1.1, exp = 1}
function Bn.add(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	local man1, man2 = val1.man, val2.man
	local exp1, exp2 = val1.exp, val2.exp
	if exp1 > exp2 then
		local diff = exp1 - exp2
		if diff > 15 then
			return val1
		end
		man2 = man2 * 10^-diff
		exp2 = exp1
	elseif exp2 > exp1 then
		local diff = exp2 - exp1
		if diff > 15 then
			return val1
		end
		man1 = man1 * 10^-diff
		exp1 = exp2
	end
	local man, exp = man1+ man2, exp1
	return Bn.new(man, exp)
end

-- can turn .add() into .sub()
function Bn.neg(val: any): BN
	val = Bn.convert(val)
	return Bn.new(-val.man, val.exp)
end

-- does the inverse of add 1 - 2 = 0 to safe keep so there will be no negatives if canNeg means add(val1, neg(val2))
function Bn.sub(val1: any, val2: any, canNeg: boolean?): BN
	canNeg = canNeg or false
	val1 = Bn.convert(val1)
	if canNeg then
		local result = Bn.add(val1, Bn.neg(val2))
		if result.man < 0 then return zero end
		return result
	end
	val2 = Bn.convert(val2)
	local man1, man2 = val1.man, val2.man
	local exp1, exp2 = val1.exp, val2.exp
	if exp1 > exp2 then
		local diff = exp1 - exp2
		if diff > 15 then
			return val1
		end
		man2 = man2 * 10^-diff
		exp2 = exp1
	elseif exp2 > exp1 then
		local diff = exp2 - exp1
		if diff > 15 then
			return val1
		end
		man1 = man1 * 10^-diff
		exp1 = exp2
	end
	local man, exp = man1 - man2, exp1
	if man < 0 then return zero end
	return Bn.new(man, exp)
end

-- mul is 10*5 for {man=5, exp = 2}
function Bn.mul(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1.man == 0 or val2.man == 0 then return zero end
	if val1.man ~= val1.man or val2.man ~= val2.man then return nan end
	if val1.exp == math.huge or val2.exp == math.huge then return inf end
	local man = val1.man * val2.man
	local exp = val1.exp + val2.exp
	return Bn.new(man, exp)
end

-- which means 1/BN.man, exp = -exp so {man = 0.1, exp = -3} is just man = 10, exp = 3
function Bn.recip(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 then return inf end
	if val.man ~= val.man or val.exp ~= val.exp then return nan end
	if val.exp == math.huge then if val.man < 0 then return zero end return zero end
	local man, exp = 1 / val.man, -val.exp
	return {man=man,exp=exp}
end

-- does the inverse of mul but can do canRecip means mul(val1, recip(val2))
function Bn.div(val1:any, val2:any, canRecip: boolean?)
	canRecip = canRecip or false
	val1 = Bn.convert(val1)
	if canRecip then
		local result = Bn.mul(val1, Bn.recip(val2))
		if result.man < 0 then return zero end
		return result
	end
	val2 = Bn.convert(val2)
	if val2.man == 0 then return inf end
	if val1.man == 0 then return zero end
	if val1.man ~= val1.man or val2.man ~= val2.man then return nan end
	if val1.exp == math.huge and val2.exp ~= math.huge then return inf end
	if val2.exp == math.huge and val1.exp ~= math.huge then return zero end
	local man = val1.man / val2.man
	local exp = val1.exp - val2.exp
	return Bn.new(man, exp)
end

--is able to compute for 10^2 which is {man = 1, exp = 2}
function Bn.pow(val1: any, val2: any, raw: boolean?): BN
	raw = raw or false
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	local man1, man2 = val1.man, val2.man
	local exp1, exp2 = val1.exp, val2.exp
	if man1 == 0 then
		if man2 == 0 then return one end
		return zero
	end
	if man1 ~= man1 or man2 ~= man2 then return nan end
	if exp1 == math.huge then
		if man2 < 0 then return zero end
		return inf
	end
	if exp2 == math.huge then
		if man1 < 0 then return zero end
		return inf
	end
	if man1 < 0 then
		local n = Bn.toNumber(val2)
		if n % 1 ~= 0 then return nan end
		local isOdd = (math.fmod(n, 2) ~= 0)
		local absBase = {man = -man1, exp = exp1}
		local res = Bn.pow(absBase, val2, raw)
		if isOdd then res.man = -res.man end
		return res
	end
	if exp2 == 0 and man2 == 1 then return one end
	if exp2 == 0 and man2 == 0 then return one end
	if exp1 == 0 and man1 == 1 then return one end
	local logVal1 = math.log10(man1) + exp1
	local log_exponent = exp2 + math.log10(math.abs(man2))
	if log_exponent > 308 then
		if logVal1 > 0 then return inf end
		if logVal1 < 0 then return zero end
		return one
	end
	local exp2pow = man2 * (10 ^ exp2)
	local L = logVal1 * exp2pow
	if L == math.huge then return inf end
	if L == -math.huge then return zero end
	local newE = math.floor(L)
	local newM = 10 ^ (L - newE)
	if newM >= 10 then
		newM = newM / 10
		newE = newE + 1
	end
	if raw then
		return {man = newM, exp = newE}
	end
	return Bn.new(newM, newE)
end

--[[
 acts like pow but instead of pow(val, 2) its just
 
pow10(mul(val, log10(2)))
]]
function Bn.pow2(val: any): BN
	val = Bn.convert(val)
	return Bn.pow10(Bn.mul(val, math.log10(2)))
end

--[[
able to compute 0.1^10 to {man = 1.2589254117941673, exp = 0} 

since Bn.new(man, exp) is able to compute exp
so exp 0.1 is just 0.258, exp 0.2 is 0.58 so man = 1 * 10^(exp % 1) 
]]
function Bn.pow10(val: any, rawPow10: boolean?): BN
	rawPow10 = rawPow10 or false
	val = Bn.convert(val)
	local exp = val.man * 10^val.exp
	local man = 1
	if rawPow10 then
		return {man = man, exp = exp}
	end
	return Bn.new(man, exp)
end

-- is the natural logfor log(val) and not log(val1, val2)
function Bn.logn(val: any): BN
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if man <= 0 then return nan end
	local logN = math.log(man) + exp * math.log(10)
	local newE = math.floor(logN)
	local newM = 10^(logN - newE)
	return Bn.new(newM, newE)
end

-- is the log10(val) so 1e10 is just 1e1
function Bn.log10(val: any): BN
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if man <= 0 then return nan end
	local logVal = math.log10(man) + exp
	if math.abs(logVal) < 10 then
		return Bn.new(logVal, 0)
	end
	local newE = math.floor(math.log10(math.abs(logVal)))
	local newM = logVal / (10^newE)
	return Bn.new(newM, newE)
end

--[[
works as logn(val) but also works as log(val1, val2)
]]
function Bn.log(val1: any, val2: any): BN
	val1 = Bn.convert(val1)
	local man1, exp1 = val1.man, val1.exp
	if val2 == nil then return Bn.logn(val1) end
	val2 = Bn.convert(val2)
	local man2, exp2 = val2.man, val2.exp
	local logNum = math.log10(man1) + exp1
	local logBase = math.log10(man2) + exp2
	if logBase == 0 then return inf end
	local res = logNum / logBase
	local newE = math.floor(res)
	local newM = 10^(res - newE)
	return Bn.new(newM, newE)
end

--[[
is basically pow but instead
man ^0.5
exp /= 2
]]
function Bn.sqrt(val: any): BN
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if man <= 0 then return nan end
	exp /= 2
	man ^= 0.5
	return Bn.new(man, exp)
end

--[[
same as sqrt but instead its man ^ (1/3) exp / 3
]]
function Bn.cbrt(val: any): BN
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if man <= 0 then return nan end
	exp /= 3
	man ^= (1/3)
	return Bn.new(man, exp)
end

--[[
basically pow(val, 1/2)
]]
function Bn.root(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	local man1, man2 = val1.man, val2.man
	local exp1, exp2 = val1.exp, val2.exp
	if man1 < 0 then
		if man2 %2 == 0 and exp2 == 0 then return nan end
	end
	return Bn.pow(val1, Bn.recip(val2))
end

--[[
able to compute for 0 or 1 or -1 for doing le, leeq, me, meeq and meeq
]]
function Bn.compare(val1: any, val2: any): number
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val1.exp ~= val2.exp then
		return (val1.exp > val2.exp) and 1 or -1
	end
	if val1.man ~= val2.man then
		return (val1.man > val2.man) and 1 or -1
	end
	return 0
end

-- computes as val1 == val2
function Bn.eq(val1: any, val2: any): boolean
	return Bn.compare(val1, val2) == 0
end

-- computes as val1 > val2
function Bn.me(val1: any, val2: any): boolean
	return Bn.compare(val1, val2) == 1
end

-- computes as val < val2
function Bn.le(val1: any, val2: any): boolean
	return Bn.compare(val1, val2) == -1
end

-- computes as val1 <= val2
function Bn.leeq(val1: any, val2: any): boolean
	local com = Bn.compare(val1, val2)
	return com < 0 or com == 0
end

-- computes as val1 >= val2
function Bn.meeq(val1: any, val2: any): boolean
	local com = Bn.compare(val1, val2)
	return com > 0 or com == 0
end

-- computes as val1 < middle and val2 < middle
function Bn.between(val1: any, val2: any, middle: any): boolean
	return Bn.leeq(val1, middle) and Bn.leeq(middle, val2)
end

--[[
helps with math.floor but for BN
]]
function Bn.floor(val: any): BN
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if man == 0 and exp == math.huge or exp ~= exp then
		return val
	end
	if exp > 0 then
		man = math.floor(man)
		return Bn.new(man, exp)
	end
	local sMan = man * 10^exp
	local floor = math.floor(sMan)
	if floor == 0 then return zero end
	local newE = math.floor(math.log10(floor))
	local newM = floor / 10^newE
	return Bn.new(newM, newE)
end

-- converts BN to readable time for s, m, h, d, w
function Bn.timeConvert(val: any): string
	local seconds = Bn.toNumber(Bn.convert(val))
	if seconds < 0 then return "0s" end
	local days = math.floor(seconds / 86400)
	local hours = math.floor((seconds % 86400) / 3600)
	local minutes = math.floor((seconds % 3600) / 60)
	local secs = seconds % 60
	local wholeSec = math.floor(secs)
	local parts = {}
	if days > 0 then table.insert(parts, days .. "d") end
	if hours > 0 then table.insert(parts, hours .. "h") end
	if minutes > 0 then table.insert(parts, minutes .. "m") end
	if wholeSec > 0 or (#parts == 0) then
		table.insert(parts, wholeSec .. "s")
	end
	if wholeSec == 0 then return 'Ready' end
	return table.concat(parts, ":")
end

-- able todo 1,000 for 1e3 and doenst do . since the encode and decode doesnt like it
function Bn.Comma(val: any, digits: number): string
	val = Bn.toNumber(val)
	local intPart = math.floor(val * 10^digits + 0.001) / 10^digits
	local str = tostring(intPart)
	local formatted = str:reverse():gsub("(%d%d%d)", "%1,"):reverse()
	formatted = formatted:gsub("^,", "")
	return formatted
end

local first = {'', 'k', 'm', 'b'}
local firstset = {"", "U","D","T","Qd","Qn","Sx","Sp","Oc","No"}
local second = {"", "De","Vt","Tg","qg","Qg","sg","Sg","Og","Ng"}
local third = {"", "Ce", "Du","Tr","Qa","Qi","Se","Si","Ot","Ni"}

function Bn.suffixPart(index: number): string
	local hund = math.floor(index/100)
	index = math.fmod(index, 100)
	local ten = math.floor(index/10)
	index = math.fmod(index, 10)
	local one = math.floor(index/1)
	return (firstset[one+1] or '') .. (second[ten+1] or '') .. (third[hund+1] or '')
end

-- acts so u can do 1k for 1e3, 1e30 -No and so on
function Bn.short(val: any, digits: number?): string
	digits = digits or 2
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if exp ~= exp then return 'NaN' end
	if exp == math.huge then return man >= 0 and 'Inf' or '-Inf' end
	if man == 0 then return '0' end
	if exp <= -3 then
		local index = math.floor(-exp / 3)
		man = math.floor(man * 100+ 0.001) / 100
		if index <= 3 then return '1/' ..man.. first[index + 1] end
		return '1/' ..man.. Bn.suffixPart(index-1)
	end
	if exp >= 3 and exp < 5 then
		return Bn.Comma(val, digits)
	end
	if exp < 3 then
		local num = Bn.toNumber(val)
		return tostring(num)
	end
	local index = math.floor(exp/3)
	local rem = exp % 3
	local scaled = man * 10^rem
	scaled = math.floor(scaled * (10^digits) + 0.001) / (10^digits)
	if index <= 3 then return scaled .. (first[index + 1] or '')	end
	return scaled .. Bn.suffixPart(index - 1)
end

-- acts like short but on a different note as in grabs exp instead so like 1e2 is just 1e2 after 1e1000 its E1k up to E100UCe
function Bn.shortE(val: any): string
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	local lf = exp % 3
	if exp ~= exp then return "NaN" end
	if exp == math.huge then return man >= 0 and "Inf" or "-Inf" end
	if man == 0 then return "0" end
	if exp < 1000 then
		man = math.floor(man * 10^lf) / 10^lf
		return man .. 'e' .. math.floor(exp)
	end
	local expBn = Bn.fromNumber(exp)
	local expStr = Bn.short(expBn)
	return 'E' .. expStr
end

-- just like toString but instead its just man .. 'e' .. exp so 1.23e1308 doesnt convert toHyperE
function Bn.toScienctific(val: BN): string
	return val.man .. 'e' .. val.exp
end

-- converts 1e1000 down to 1e1e3
function Bn.toHyperE(val: any): string
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if exp ~= exp then return "NaN" end
	if exp == math.huge then return man >= 0 and "Inf" or "-Inf" end
	if man == 0 then return "0" end
	local function hyperE(e: number): string
		if e<= 308 then
			return tostring(math.floor(e))
		end
		local top = math.floor(math.log10(e) * 100 + 0.001) / 100
		local frac = e/ 10^top
		return math.floor(frac * 100 + 0.001) / 100 ..'e' .. hyperE(top)
	end
	return man ..'e' .. hyperE(exp)
end

--formats short(1e3) to 1k, shortE(1e1e3) acts as E1k and toHyperE(1e1e61) is just 1e1e61
function Bn.format(val: any, digits: number?): string
	if Bn.meeq(val, '1e1e20') then
		return Bn.toHyperE(val)
	elseif Bn.meeq(val, '1e3e3') then
		return Bn.shortE(val)
	end
	return Bn.short(val, digits)
end

-- gets the lowest like 1, 1.5e10 only grabs 1
function Bn.min<T...>(...: T...): BN
	local args = {...}
	if #args == 0 then return zero end
	local best = Bn.convert(args[1])
	for i = 2, #args do
		local val = Bn.convert(args[i])
		if Bn.compare(val, best) < 0 then
			best = val
		end
	end
	return best
end

-- gets the best out of the ... so if u have 1, 5, '1e50' it gets the '1e50'
function Bn.max(val1: any, val2: any): BN
	if val1.exp ~= val2.exp then
		return (val1.exp > val2.exp) and val1 or val2
	end
	return (val1.man > val2.man) and val1 or val2
end

-- clamps val = 0, min {man=0, exp=0}, max = BN max
function Bn.clamp(val: any, min: any, max: any): BN
	if Bn.compare(min, max) > 0 then
		min, max = max, min
	end
	if Bn.compare(val, min) < 0 then
		return min
	elseif Bn.compare(val, max) > 0 then
		return max
	end
	return val
end

function Bn.ceil(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 and val.exp  == math.huge or val.exp ~= val.exp then
		return val
	end
	local man, exp = val.man, val.exp
	if exp > 0 then
		man = math.ceil(man)
		return {man =man, exp = exp}
	end
	local sMan = man * 10^exp
	local ceil = math.ceil(sMan)
	if ceil == 0 then return zero end
	local nExp = math.floor(math.log10(ceil))
	local nMan = ceil/10^nExp
	return Bn.new(nMan, nExp)
end

-- rounds BN to its nearest val like {man = 1.46, exp = 2} rounds to {man = 1.5, exp = 2}
function Bn.round(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 and val.exp  == math.huge or val.exp ~= val.exp then
		return val
	end
	local man, exp = val.man, val.exp
	if exp > 0 then
		man = math.round(man + 0.001)
		return {man = man, exp = exp}
	end
	local sMan = man * 10^exp
	local round = math.floor(sMan + 0.001)
	if round == 0 then return zero end
	local nExp = math.floor(math.log10(round))
	local nMan = round / 10^nExp
	return Bn.new(nMan, nExp)
end

-- like math.exp
function Bn.exp(val: any): BN
	val = Bn.convert(val)
	if val.man == 0 then return one end
	if val.exp == math.huge then return inf end
	if val.exp ~= val.exp then return nan end
	local pow = Bn.mul(val, 0.4342944819032518)
	return Bn.pow10(pow)
end

--basically like math.mod but for BN
function Bn.mod(val1: any, val2: any): BN
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val2.man == 0 then return nan end
	local cmp = Bn.compare(val1, val2)
	if cmp < 0 then
		return val1
	elseif cmp == 0 then
		return zero
	end
	local ratio = Bn.div(val1, val2)
	local floor = Bn.floor(ratio)
	local prod = Bn.mul(floor, val2)
	return Bn.sub(val1, prod)
end

--basically like math.modf but for BN
function Bn.modf(val: any): (BN, BN)
	val = Bn.convert(val)
	if val.man == 0 and val.exp == math.huge or val.exp ~= val.exp then
		return val, zero
	end
	local int = Bn.floor(val)
	local frac = Bn.sub(val, int)
	return int ,frac
end

--basically like math.fmod but for BN
function Bn.fmod(val1: any, val2: any): BN
	local q = Bn.div(val1, val2)
	local qF = Bn.floor(q)
	local prod = Bn.mul(qF, val2)
	return Bn.sub(val1, prod)
end

--computes a log growth step: log10(val + 10^(sqrt(log10(val+10))))
function Bn.HyperRootLog(val: any): BN
	val = Bn.log10(val)
	val = Bn.sqrt(val)
	return Bn.new(val.man, val.exp)
end

--creates it so if 100/1000 shows as 10%
function Bn.Percent(val1: any, val2: any): string
	val1, val2 = Bn.convert(val1), Bn.convert(val2)
	if val2.man == 0 then return '100%' end
	local ratio = Bn.div(val1, val2)
	local percent = Bn.mul(ratio, 100)
	if percent.man < 0 or percent.exp < 0 then
		return '0%'
	end
	local hund = Bn.fromNumber(1e2)
	if Bn.meeq(percent, hund) then return '100%' end
	return Bn.format(percent) .. '%'
end

local expScale = 1e6
local manScale = 1e6

-- able to compute as 1e3 to 6.02059991328e11 in encode
function Bn.lbencode(val: any): number
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	if man == 0 then return 4e18 end
	local sign = man < 0 and -1 or 1
	man = math.abs(man)
	local expLog = math.log10(exp + 1)
	local expInt = math.floor(expLog * expScale + 0.5)
	local manLog = math.log10(man)
	local manInt = math.floor(manLog * manScale + 0.5)
	return sign * (expInt * 1e6 + manInt)
end

-- converts encode like 6.02059991328e11 back to 1e3
function Bn.lbdecode(val: number): BN
	if val == 4e18 then return zero end
	local sign = (val < 0) and -1 or 1
	val = math.abs(val)
	local expPart = math.floor(val/1e6)
	local manPart = val % 1e6
	local exp = math.floor(10^(expPart / expScale) - 1+0.001)
	local man = 10^(manPart / manScale)
	return Bn.new(man * sign, exp)
end

-- makes sure that 1e30 is the max lets say 1e3 is ur cash rn but u had 1e30 Cash that oldData will be stored as its max
function Bn.encodeData(val: any, oldData: any): number
	local new = Bn.convert(val)
	if oldData == nil then return Bn.lbencode(val) end
	if oldData then
		local old = Bn.lbdecode(oldData)
		new = Bn.max(old, new)
	end
	return Bn.lbencode(new)
end

local hnNaN: HN = {man = 1, layer = 0/0, exp = 0/0}
local hnZero: HN = {man = 0, layer = 0, exp = 0}

-- Suffix table creator
function Bn.newSuffixTable(firstSet: {string}, secondSet: {string}, thirdSet: {string})
	local T = {firstSet = firstSet, secondSet = secondSet, thirdSet = thirdSet}
	function T:customSuffix(index: number): string
		local hund = math.floor(index / 100)
		index %= 100
		local ten = math.floor(index / 10)
		index %= 10
		local one = index
		return (self.firstSet[one + 1]:: string or "") .. (self.secondSet[ten + 1]:: string or "") .. (self.thirdSet[hund + 1]:: string or "")
	end
	return T
end
--[[
must use the customSuffix function to access this
but have it as ur own if u want as example
firstSet = {"", "U","D","T","Qd","Qn","Sx","Sp","Oc","No"}
Second = {"", "De","Vt","Tg","qg","Qg","sg","Sg","Og","Ng"}
Third = {"", "Ce", "Du","Tr","Qa","Qi","Se","Si","Ot","Ni"}
 ]]
function Bn.customShort(val: any, customSuffix , digits: number?): string
	digits = digits or 0
	val = Bn.convert(val)
	local man, exp = val.man, val.exp
	local firstSet, secondSet, thirdSet = customSuffix.firstSet, customSuffix.secondSet, customSuffix.thirdSet
	if exp ~= exp then return 'NaN' end
	if exp == math.huge then return man >= 0 and 'Inf' or '-Inf' end
	if man == 0 then return '0' end
	if exp < -4 then
		man = math.floor(man * 100 +0.001) / 100
		local index = math.floor(-exp / 3)
		if index <= #first then
			return '1/' ..man.. first[index + 1]
		end
		return '1/' ..man.. Bn.newSuffixTable(firstSet, secondSet, thirdSet):customSuffix(index)
	end
	if exp >= 3 and exp < 9 then
		return Bn.Comma(val, digits)
	end
	if exp < 3 then
		local num = Bn.toNumber(val)
		return tostring(math.floor(num * 100 + 0.001) / 100)
	end
	local index = math.floor(exp/3)
	if index < #first then
		return man .. first[index+1] or ''
	end
	local suffix = index - 1
	return man .. Bn.newSuffixTable(firstSet, secondSet, thirdSet):customSuffix(suffix)
end

-- able to abs neg(val) == -1.325e3 back to 1.325e3 if not then stays 1.325e3
function Bn.abs(val: any): BN
	val = Bn.convert(val)
	if val.man < 0 then
		return Bn.new(-val.man, val.exp)
	end
	return val
end

--[[
helper function to help convert BN to HN
]]
function hnnormalize(man: number, layer: number, exp: number): HN
	if man == 0 then return {man = 0, layer = 0, exp = 0} end
	if man ~= man or exp ~= exp then return {man = 1, layer = 0, exp = 0/0} end
	local sign = 1
	if man < 0 then
		sign = -1
		man = -man
	end
	local logMan = math.floor(math.log10(man))
	man /= 10^logMan
	exp += logMan
	if exp >= 308 then
		exp = math.log10(exp)
		layer += 1
	end
	if layer > 0 and exp < 1 then
		exp = 10^exp
		layer -= 1
	end
	if layer == math.huge or exp == math.huge then
		return {man = 1, layer = math.huge, exp = math.huge}
	end
	if layer < 0 then return {man = 1, layer = 0, exp = 0/0} end
	man *= sign
	return {man = man, layer = layer, exp = exp}
end

-- able to convert BN to HN which will last longer then BN
function Bn.toHN(val: any): HN
	val = Bn.convert(val)
	return hnnormalize(val.man, 0, val.exp)
end

return Bn
