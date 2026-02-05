--!strict
--optimized 2
local Bnum = {}
type BN = {number}

function Bnum.fromNumber(val: number): BN
	local man, exp
	if val == math.huge then return {1, math.huge} end
	if val == -math.huge then return {-1, math.huge} end
	if val == 0 then return {0, 0} end
	exp = math.floor(math.log10(math.abs(val)))
	man = val / math.pow(10, exp)
	return {man, exp}
end

function Bnum.toNumber(val: any): number
	local man1: number, exp1: number
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(math.abs(val)))
			man1, exp1 = val/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(val):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man1, exp1 = val[1], val[2]
		end
		if #val == 2 then
			man1, exp1 = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(math.abs(val[1])))
				man1, exp1 = val/math.pow(10, exp), exp
			end
		end
	end
	if exp1 > 308 then return math.huge end
	local scale = man1 * math.pow(10, exp1)
	scale = math.floor(scale * 100 + 0.001) / 100
	return scale
end

function Bnum.add(val1: any, val2: any): BN
	local man1: number, exp1: number = 0, 0
	local man2: number, exp2: number = 0, 0
	local types = type(val1)
	if types == 'number' then
		if val1 == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(val1))
			man1, exp1 = val1/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val1, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val1, 1, e-1)):: number, tonumber(string.sub(val1, e+1, -1)):: number
		else
			local number: number = tonumber(val1):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val1 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val1, 'to', {val1[1], val1[2]},' to BN\n    ', 'which is from',Bnum.toStr({val1[1],val1[2]}), 'to:', {val1[1], val1[2]})
			man1, exp1 = val1[1], val1[2]
		end
		if #val1 == 2 then
			man1, exp1 = val1[1], val1[2]
		elseif #val1 == 1 then
			if val1[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val1[1]))
				man1, exp1 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(val2)
	if t == 'number' then
		if val2 == 0 then
			man2, exp2 = 0,0
		else
			local exp = math.floor(math.log10(val2))
			man2, exp2 = val2/math.pow(10, exp), exp
		end
	elseif t == 'string' then
		local e = string.find(val2, 'e')
		if e then
			man2, exp2 = tonumber(string.sub(val2, 1, e-1)):: number, tonumber(string.sub(val2, e+1, -1)):: number
		else
			local number: number = tonumber(val2):: number
			if number == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man2, exp2 = number/math.pow(10, exp), exp
			end
		end
	elseif t == 'table' then
		if #val2 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val2, 'to', {val2[1], val2[2]},' to BN\n    ', 'which is from',Bnum.toStr({val2[1], val2[2]}), 'to:', {val2[1], val2[2]})
			man2, exp2 = val2[1], val2[2]
		end
		if #val2 == 2 then
			man2, exp2 = val2[1], val2[2]
		elseif #val2 == 1 then
			if val2[1] == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(val2[1]))
				man2, exp2 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	if exp1 > exp2 then
		local diff = exp1 - exp2
		if diff > 15 then
			return {man1, exp1}
		end
		man2 = man2 * math.pow(10, -diff)
		exp2 = exp1
	elseif exp2 > exp1 then
		local diff = exp2 - exp1
		if diff > 15 then
			return {man1, exp1}
		end
		man1 = man1 * math.pow(10, -diff)
		exp1 = exp2
	end
	local man = man1 + man2
	if man == 0 then
		return {0, 0}
	end
	local expShift = math.floor(math.log10(math.abs(man)))
	man = man / math.pow(10, expShift)
	return {man, exp1 + expShift}
end

function Bnum.sub(val1: any, val2: any, shouldNotGoBelowZero: boolean?): BN
	local man1: number, exp1: number = 0, 0
	local man2: number, exp2: number = 0, 0
	local types = type(val1)
	if types == 'number' then
		if val1 == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(val1))
			man1, exp1 = val1/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val1, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val1, 1, e-1)):: number, tonumber(string.sub(val1, e+1, -1)):: number
		else
			local number: number = tonumber(val1):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val1 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val1, 'to', {val1[1], val1[2]},' to BN\n    ', 'which is from',Bnum.toStr({val1[1],val1[2]}), 'to:', {val1[1], val1[2]})
			man1, exp1 = val1[1], val1[2]
		end
		if #val1 == 2 then
			man1, exp1 = val1[1], val1[2]
		elseif #val1 == 1 then
			if val1[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val1[1]))
				man1, exp1 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(val2)
	if t == 'number' then
		if val2 == 0 then
			man2, exp2 = 0,0
		else
			local exp = math.floor(math.log10(val2))
			man2, exp2 = val2/math.pow(10, exp), exp
		end
	elseif t == 'string' then
		local e = string.find(val2, 'e')
		if e then
			man2, exp2 = tonumber(string.sub(val2, 1, e-1)):: number, tonumber(string.sub(val2, e+1, -1)):: number
		else
			local number: number = tonumber(val2):: number
			if number == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man2, exp2 = number/math.pow(10, exp), exp
			end
		end
	elseif t == 'table' then
		if #val2 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val2, 'to', {val2[1], val2[2]},' to BN\n    ', 'which is from',Bnum.toStr({val2[1], val2[2]}), 'to:', {val2[1], val2[2]})
			man2, exp2 = val2[1], val2[2]
		end
		if #val2 == 2 then
			man2, exp2 = val2[1], val2[2]
		elseif #val2 == 1 then
			if val2[1] == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(val2[1]))
				man2, exp2 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	shouldNotGoBelowZero = shouldNotGoBelowZero or false
	if exp1 > exp2 then
		local diff = exp1 - exp2
		if diff > 15 then
			return val1
		end
		man2 = man2 * math.pow(10, -diff)
		exp2 = exp1
	elseif exp2 > exp1 then
		local diff = exp2 - exp1
		if diff > 15 then
			return val1
		end
		man1 = man1 * math.pow(10, -diff)
		exp1 = exp2
	end
	local man = man1 - man2
	if man == 0 then
		return {0, 0}
	end
	if shouldNotGoBelowZero and man < 0 then
		return {0, 0}
	end
	local expShift = math.floor(math.log10(math.abs(man)))
	man = man / math.pow(10, expShift)
	return {man, exp1 + expShift}
end

function Bnum.mul(val1: any, val2: any): BN
	local man1: number, exp1: number = 0, 0
	local man2: number, exp2: number = 0, 0
	local types = type(val1)
	if types == 'number' then
		if val1 == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(val1))
			man1, exp1 = val1/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val1, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val1, 1, e-1)):: number, tonumber(string.sub(val1, e+1, -1)):: number
		else
			local number: number = tonumber(val1):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val1 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val1, 'to', {val1[1], val1[2]},' to BN\n    ', 'which is from',Bnum.toStr({val1[1],val1[2]}), 'to:', {val1[1], val1[2]})
			man1, exp1 = val1[1], val1[2]
		end
		if #val1 == 2 then
			man1, exp1 = val1[1], val1[2]
		elseif #val1 == 1 then
			if val1[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val1[1]))
				man1, exp1 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(val2)
	if t == 'number' then
		if val2 == 0 then
			man2, exp2 = 0,0
		else
			local exp = math.floor(math.log10(val2))
			man2, exp2 = val2/math.pow(10, exp), exp
		end
	elseif t == 'string' then
		local e = string.find(val2, 'e')
		if e then
			man2, exp2 = tonumber(string.sub(val2, 1, e-1)):: number, tonumber(string.sub(val2, e+1, -1)):: number
		else
			local number: number = tonumber(val2):: number
			if number == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man2, exp2 = number/math.pow(10, exp), exp
			end
		end
	elseif t == 'table' then
		if #val2 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val2, 'to', {val2[1], val2[2]},' to BN\n    ', 'which is from',Bnum.toStr({val2[1], val2[2]}), 'to:', {val2[1], val2[2]})
			man2, exp2 = val2[1], val2[2]
		end
		if #val2 == 2 then
			man2, exp2 = val2[1], val2[2]
		elseif #val2 == 1 then
			if val2[1] == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(val2[1]))
				man2, exp2 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local man = man1 * man2
	local expSum = exp1 + exp2
	if man == 0 then
		return {0, 0}
	end
	local expShift = math.floor(math.log10(math.abs(man)))
	man = man / math.pow(10, expShift)
	return {man, expSum + expShift}
end

function Bnum.div(val1: any, val2: any): BN
	local man1: number, exp1: number = 0, 0
	local man2: number, exp2: number = 0, 0
	local types = type(val1)
	if types == 'number' then
		if val1 == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(val1))
			man1, exp1 = val1/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val1, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val1, 1, e-1)):: number, tonumber(string.sub(val1, e+1, -1)):: number
		else
			local number: number = tonumber(val1):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val1 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val1, 'to', {val1[1], val1[2]},' to BN\n    ', 'which is from',Bnum.toStr({val1[1],val1[2]}), 'to:', {val1[1], val1[2]})
			man1, exp1 = val1[1], val1[2]
		end
		if #val1 == 2 then
			man1, exp1 = val1[1], val1[2]
		elseif #val1 == 1 then
			if val1[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val1[1]))
				man1, exp1 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(val2)
	if t == 'number' then
		if val2 == 0 then
			man2, exp2 = 0,0
		else
			local exp = math.floor(math.log10(val2))
			man2, exp2 = val2/math.pow(10, exp), exp
		end
	elseif t == 'string' then
		local e = string.find(val2, 'e')
		if e then
			man2, exp2 = tonumber(string.sub(val2, 1, e-1)):: number, tonumber(string.sub(val2, e+1, -1)):: number
		else
			local number: number = tonumber(val2):: number
			if number == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man2, exp2 = number/math.pow(10, exp), exp
			end
		end
	elseif t == 'table' then
		if #val2 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val2, 'to', {val2[1], val2[2]},' to BN\n    ', 'which is from',Bnum.toStr({val2[1], val2[2]}), 'to:', {val2[1], val2[2]})
			man2, exp2 = val2[1], val2[2]
		end
		if #val2 == 2 then
			man2, exp2 = val2[1], val2[2]
		elseif #val2 == 1 then
			if val2[1] == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(val2[1]))
				man2, exp2 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local man = man1 / man2
	local expSum = exp1 - exp2
	if man == 0 then
		return {0, 0}
	end
	local expShift = math.floor(math.log10(math.abs(man)))
	man = man / math.pow(10, expShift)
	return {man, expSum + expShift}
end

function Bnum.toStr(val: BN): string
	if val[2] >= 308 then
		local Eexp = math.floor(math.log10(val[2]))
		return val[1] .. 'e' .. val[2]/math.pow(10, Eexp) .. 'e' .. Eexp
	end
	return val[1] .. 'e' .. val[2]
end

function Bnum.neg(val: any): BN
	local man1: number, exp1: number
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(math.abs(val)))
			man1, exp1 = val/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man1, exp1 = val[1], val[2]
		end
		if #val == 2 then
			man1, exp1 = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(math.abs(val[1])))
				man1, exp1 = val/math.pow(10, exp), exp
			end
		end
	end
	man1 = -man1
	return {man1, exp1}
end

function Bnum.pow(val1: any, val2: any): BN
	local man1: number, exp1: number = 0, 0
	local man2: number, exp2: number = 0, 0
	local types = type(val1)
	if types == 'number' then
		if val1 == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(val1))
			man1, exp1 = val1/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val1, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val1, 1, e-1)):: number, tonumber(string.sub(val1, e+1, -1)):: number
		else
			local number: number = tonumber(val1):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val1 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val1, 'to', {val1[1], val1[2]},' to BN\n    ', 'which is from',Bnum.toStr({val1[1],val1[2]}), 'to:', {val1[1], val1[2]})
			man1, exp1 = val1[1], val1[2]
		end
		if #val1 == 2 then
			man1, exp1 = val1[1], val1[2]
		elseif #val1 == 1 then
			if val1[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val1[1]))
				man1, exp1 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(val2)
	if t == 'number' then
		if val2 == 0 then
			man2, exp2 = 0,0
		else
			local exp = math.floor(math.log10(val2))
			man2, exp2 = val2/math.pow(10, exp), exp
		end
	elseif t == 'string' then
		local e = string.find(val2, 'e')
		if e then
			man2, exp2 = tonumber(string.sub(val2, 1, e-1)):: number, tonumber(string.sub(val2, e+1, -1)):: number
		else
			local number: number = tonumber(val2):: number
			if number == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man2, exp2 = number/math.pow(10, exp), exp
			end
		end
	elseif t == 'table' then
		if #val2 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val2, 'to', {val2[1], val2[2]},' to BN\n    ', 'which is from',Bnum.toStr({val2[1], val2[2]}), 'to:', {val2[1], val2[2]})
			man2, exp2 = val2[1], val2[2]
		end
		if #val2 == 2 then
			man2, exp2 = val2[1], val2[2]
		elseif #val2 == 1 then
			if val2[1] == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(val2[1]))
				man2, exp2 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local p = man2 * math.pow(10, exp2)
	if p == 0 then return {1, 0} end
	if man1 == 0 then return {0, 0} end
	if man1 < 0 then
		if p % 1 ~= 0 then
			return {0/0, 0}
		end
		man1 = -man1
		local sign = (p % 2 == 0) and 1 or -1
		local l = (math.log10(man1) + exp1) * p
		if l == math.huge then return {sign, math.huge} end
		local E = math.floor(l)
		return {sign * math.pow(10, l-E), E}
	end
	local l = (math.log10(man1) + exp1) * p
	if l == math.huge then return {1, math.huge} end
	if l == -math.huge then return {0, -math.huge} end
	local E = math.floor(l)
	return {math.pow(10,l-E), E}
end

function Bnum.pow10(val: any): BN
	local man1: number, exp1: number
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(math.abs(val)))
			man1, exp1 = val/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man1, exp1 = val[1], val[2]
		end
		if #val == 2 then
			man1, exp1 = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val[1]))
				man1, exp1 = val[1]/math.pow(10, exp), exp
			end
		end
	end
	local exp = man1*math.pow(10, exp1)
	local frac = exp%1
	local man = 1
	if frac ~= 0 then
		man*= math.pow(10, frac)
		exp -= frac
	end
	return {man, exp}
end

function Bnum.sqrt(val: any): BN
	local man1: number, exp1: number
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(math.abs(val)))
			man1, exp1 = val/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man1, exp1 = val[1], val[2]
		end
		if #val == 2 then
			man1, exp1 = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val[1]))
				man1, exp1 = val[1]/math.pow(10, exp), exp
			end
		end
	end
	if man1 <= 0 then return {0/0, 0} end
	man1 = math.sqrt(man1)
	exp1 /= 2
	local exp = math.floor(math.log10(man1))
	return {man1/math.pow(10, exp), exp}
end

function Bnum.cbrt(val: any): BN
	local man1: number, exp1: number
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(math.abs(val)))
			man1, exp1 = val/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man1, exp1 =0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man1, exp1 = val[1], val[2]
		end
		if #val == 2 then
			man1, exp1 = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val[1]))
				man1, exp1 = val[1]/math.pow(10, exp), exp
			end
		end
	end
	if man1 <= 0 then return {0/0, 0} end
	man1 ^= (1/3)
	exp1 /= 3
	local exp = math.floor(math.log10(man1))
	man1/= math.pow(10, exp)
	exp1 += exp
	return {man1, exp1}
end

function Bnum.logn(val: any): BN
	local man, exp
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man, exp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			man, exp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man, exp =0, 0
			else
				local exp1 = math.floor(math.log10(number:: number))
				man, exp = number/math.pow(10, exp1), exp1
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man, exp = val[1], val[2]
		end
		if #val == 2 then
			man, exp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(val[1]))
				man, exp = val[1]/math.pow(10, exp1), exp1
			end
		end
	end
	if man <= 0 then return {0/0, 0} end
	local logN = math.log(man) + exp * 2.302585092994046
	local exp = math.floor(math.log10(logN))
	man, exp = logN/math.pow(10, exp), exp
	return {man, exp}
end

function Bnum.log10(val: any): BN
	local man, exp
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man, exp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			man, exp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man, exp =0, 0
			else
				local exp1 = math.floor(math.log10(number:: number))
				man, exp = number/math.pow(10, exp1), exp1
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man, exp = val[1], val[2]
		end
		if #val == 2 then
			man, exp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(val[1]))
				man, exp = val[1]/math.pow(10, exp1), exp1
			end
		end
	end
	if man <= 0 then return {0/0, 0} end
	local logVal = math.log10(man) + exp
	if math.abs(logVal) < 10 then
		local exp = math.floor(math.log10(logVal))
		man, exp = logVal/math.pow(10, exp), exp
		return {man, exp}
	end
	local expShift = math.floor(math.log10(math.abs(logVal)))
	logVal = logVal / math.pow(10, expShift)
	return {logVal, exp + expShift}
end

function Bnum.log(val1: any, val2: any): BN
	if not val2 then return Bnum.logn(val1) end
	return Bnum.div(Bnum.log10(val1), Bnum.log10(val2))
end

function Bnum.root(val1: any, val2: any): BN
	local man1: number, exp1: number = 0, 0
	local man2: number, exp2: number = 0, 0
	local types = type(val1)
	if types == 'number' then
		if val1 == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(val1))
			man1, exp1 = val1/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val1, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val1, 1, e-1)):: number, tonumber(string.sub(val1, e+1, -1)):: number
		else
			local number: number = tonumber(val1):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val1 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val1, 'to', {val1[1], val1[2]},' to BN\n    ', 'which is from',Bnum.toStr({val1[1],val1[2]}), 'to:', {val1[1], val1[2]})
			man1, exp1 = val1[1], val1[2]
		end
		if #val1 == 2 then
			man1, exp1 = val1[1], val1[2]
		elseif #val1 == 1 then
			if val1[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val1[1]))
				man1, exp1 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(val2)
	if t == 'number' then
		if val2 == 0 then
			man2, exp2 = 0,0
		else
			local exp = math.floor(math.log10(val2))
			man2, exp2 = val2/math.pow(10, exp), exp
		end
	elseif t == 'string' then
		local e = string.find(val2, 'e')
		if e then
			man2, exp2 = tonumber(string.sub(val2, 1, e-1)):: number, tonumber(string.sub(val2, e+1, -1)):: number
		else
			local number: number = tonumber(val2):: number
			if number == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man2, exp2 = number/math.pow(10, exp), exp
			end
		end
	elseif t == 'table' then
		if #val2 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val2, 'to', {val2[1], val2[2]},' to BN\n    ', 'which is from',Bnum.toStr({val2[1], val2[2]}), 'to:', {val2[1], val2[2]})
			man2, exp2 = val2[1], val2[2]
		end
		if #val2 == 2 then
			man2, exp2 = val2[1], val2[2]
		elseif #val2 == 1 then
			if val2[1] == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(val2[1]))
				man2, exp2 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	if man1 < 0 then
		if man2 %2 == 0 and exp2 == 0 then return {0/0, 0} end
	end
	local l = math.log10(man1) + exp1
	local div = man2*math.pow(10, exp2)
	local res = l / div
	local newE = math.floor(res)
	local newM = math.pow(10, res-newE)
	return {newM, newE}
end

function Bnum.compare(val1: any, val2: any): number
	local man1: number, exp1: number = 0, 0
	local man2: number, exp2: number = 0, 0
	local types = type(val1)
	if types == 'number' then
		if val1 == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(val1))
			man1, exp1 = val1/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val1, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val1, 1, e-1)):: number, tonumber(string.sub(val1, e+1, -1)):: number
		else
			local number: number = tonumber(val1):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val1 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val1, 'to', {val1[1], val1[2]},' to BN\n    ', 'which is from',Bnum.toStr({val1[1],val1[2]}), 'to:', {val1[1], val1[2]})
			man1, exp1 = val1[1], val1[2]
		end
		if #val1 == 2 then
			man1, exp1 = val1[1], val1[2]
		elseif #val1 == 1 then
			if val1[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val1[1]))
				man1, exp1 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(val2)
	if t == 'number' then
		if val2 == 0 then
			man2, exp2 = 0,0
		else
			local exp = math.floor(math.log10(val2))
			man2, exp2 = val2/math.pow(10, exp), exp
		end
	elseif t == 'string' then
		local e = string.find(val2, 'e')
		if e then
			man2, exp2 = tonumber(string.sub(val2, 1, e-1)):: number, tonumber(string.sub(val2, e+1, -1)):: number
		else
			local number: number = tonumber(val2):: number
			if number == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man2, exp2 = number/math.pow(10, exp), exp
			end
		end
	elseif t == 'table' then
		if #val2 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val2, 'to', {val2[1], val2[2]},' to BN\n    ', 'which is from',Bnum.toStr({val2[1], val2[2]}), 'to:', {val2[1], val2[2]})
			man2, exp2 = val2[1], val2[2]
		end
		if #val2 == 2 then
			man2, exp2 = val2[1], val2[2]
		elseif #val2 == 1 then
			if val2[1] == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(val2[1]))
				man2, exp2 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	if man1 ~= man1 or man2 ~= man2 then	return 0 end
	if man1 == 0 and man2 == 0 then
		return 0
	elseif man1 == 0 then
		return (man2 > 0) and -1 or 1
	elseif man2 == 0 then
		return (man1 > 0) and 1 or -1
	end
	local sign1 = (man1 < 0) and -1 or 1
	local sign2 = (man2 < 0) and -1 or 1
	if sign1 ~= sign2 then return (sign1 > sign2) and 1 or -1 end
	if exp1 ~= exp2 then
		if sign1 > 0 then
			return (exp1 > exp2) and 1 or -1
		else
			return (exp1 > exp2) and -1 or 1
		end
	end
	if man1 ~= man2 then
		return (man1 > man2) and 1 or -1
	end
	return 0
end

function Bnum.eq(val1: any, val2: any): boolean
	return Bnum.compare(val1, val2) == 0
end

function Bnum.le(val1: any, val2: any): boolean
	return Bnum.compare(val1, val2) == -1
end

function Bnum.me(val1: any, val2: any): boolean
	return Bnum.compare(val1, val2) == 1
end

function Bnum.leeq(val1: any, val2: any): boolean
	return Bnum.compare(val1, val2) <= 0
end

function Bnum.meeq(val1: any, val2: any): boolean
	return Bnum.compare(val1, val2) >= 0
end

function Bnum.HyperRootLog(val: any): BN
	local man1: number, exp1: number
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(math.abs(val)))
			man1, exp1 = val/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number = math.floor(math.log10(val))
			if number == 0 then
				man1, exp1 =0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man1, exp1 = val[1], val[2]
		end
		if #val == 2 then
			man1, exp1 = val[1], val[2]
		elseif #val == 1 then
			local exp = math.floor(math.log10(math.abs(val)))
			man1, exp1 = val/math.pow(10, exp), exp
		end
	end
	local x = math.log10(man1) + exp1
	local y = math.sqrt(x)
	local newE = math.floor(math.log10(y))
	local newM = y / math.pow(10, newE)
	return {newM, newE}
end

function Bnum.abs(val: any): BN
	local man1: number, exp1: number
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(math.abs(val)))
			man1, exp1 = val/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man1, exp1 =0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man1, exp1 = val[1], val[2]
		end
		if #val == 2 then
			man1, exp1 = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val[1]))
				man1, exp1 = val[1]/math.pow(10, exp), exp
			end
		end
	end
	local abs = man1 < 0 and -man1 or man1
	return {abs, exp1}
end

local first = {'', 'k', 'm', 'b'}
local firstset = {"", "U","D","T","Qd","Qn","Sx","Sp","Oc","No"}
local second   = {"", "De","Vt","Tg","qg","Qg","sg","Sg","Og","Ng"}
local third    = {"", "Ce","Du","Tr","Qa","Qi","Se","Si","Ot","Ni"}

function Bnum.format(val: any,digits: number?,hyperAt: number?): string
	local man, exp
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man, exp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			man, exp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(number:: number))
				man, exp = number/math.pow(10, exp1), exp1
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man, exp = val[1], val[2]
		end
		if #val == 2 then
			man, exp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(val[1]))
				man, exp1 = val[1]/math.pow(10, exp1), exp1
			end
		end
	end
	if exp ~= exp then return "NaN" end
	if exp == math.huge then return man >= 0 and "Inf" or "-Inf" end
	digits = digits or 2
	hyperAt = hyperAt or 1e20
	if exp >= hyperAt then
		local eexp = math.floor(math.log10(exp))
		return man .. "e" .. Bnum.format({exp / 10^eexp, eexp}, digits, hyperAt)
	end
	if exp >= 3e3 then
		local lf = exp % 3
		local m = math.floor(man * 10^lf) / 10^lf
		return m .. "e" .. exp
	end
	if exp >= 3 and exp < 6 then
		man *= math.pow(10, exp)
		man = math.floor(man * math.pow(10, digits) + 0.001) / math.pow(10, digits)
		local str = tostring(man)
		local formatted = str:reverse():gsub("(%d%d%d)", "%1,"):reverse()
		formatted = formatted:gsub("^,", "")
		return formatted
	end
	if exp >= 6 then
		local index = math.floor(exp / 3)
		local rem = exp % 3
		local scaled = man * math.pow(10, rem)
		local round = math.pow(10, digits)
		scaled = math.floor(scaled * round + 0.001) / round
		if index <= 3 then
			return scaled .. first[index + 1]
		end
		local i = index - 1
		local a =  i % 10
		local b = (i // 10) % 10
		local c = (i // 100) % 10
		return scaled .. firstset[a+1] .. second[b+1] .. third[c+1]
	end
	if exp < -1 then
		local index = math.floor(-exp / 3)
		local rem = -exp % 3
		local scaled = man * math.pow(10, rem)
		local round = math.pow(10, digits)
		scaled = math.floor(scaled * round + 0.001) / round
		if index <= 3 then
			return 1 .. '/' .. scaled .. first[index + 1]
		end
		local i = index - 1
		local a =  i % 10
		local b = (i // 10) % 10
		local c = (i // 100) % 10
		return '1/' .. scaled .. firstset[a+1] .. second[b+1] .. third[c+1]
	end
	local scale = man * math.pow(10, exp)
	scale = math.floor(scale * 100 + 0.001) / 100
	return tostring(scale)
end

function Bnum.min<T...>(...: T...): BN
	local count = select("#", ...)
	if count == 0 then
		return {0, 0}
	end
	local bestMan: number, bestExp: number
	for i = 1, count do
		local val: any = select(i, ...)
		local man, exp
		local types = type(val)
		if types == 'number' then
			if val == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(math.abs(val)))
				man, exp = val/math.pow(10, exp1), exp1
			end
		elseif types == 'string' then
			local e = string.find(val, 'e')
			if e then
				man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
			else
				local number: number = tonumber(math.abs(val)):: number
				if number == 0 then
					man, exp = 0, 0
				else
					local exp1 = math.floor(math.log10(number:: number))
					man, exp = number/math.pow(10, exp1), exp1
				end
			end
		elseif types == 'table' then
			if #val >= 3 then 
				warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
				warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
				man, exp = val[1], val[2]
			end
			if #val == 2 then
				man, exp = val[1], val[2]
			elseif #val == 1 then
				if val[1] == 0 then
					man, exp = 0, 0
				else
					local exp1 = math.floor(math.log10(val[1]))
					man, exp = val[1]/math.pow(10, exp1), exp1
				end
			end
		end
		if i == 1 then
			bestMan, bestExp = man, exp
		else
			if exp < bestExp or (exp == bestExp and man < bestMan) then
				bestMan, bestExp = man, exp
			end
		end
	end
	return {bestMan, bestExp}
end

function Bnum.max<T...>(...: T...): BN
	local count = select("#", ...)
	if count == 0 then
		return {0, 0}
	end
	local bestMan: number, bestExp: number
	for i = 1, count do
		local val: any = select(i, ...)
		local man, exp
		local types = type(val)
		if types == 'number' then
			if val == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(math.abs(val)))
				man, exp = val/math.pow(10, exp1), exp1
			end
		elseif types == 'string' then
			local e = string.find(val, 'e')
			if e then
				man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
			else
				local number: number = tonumber(math.abs(val)):: number
				if number == 0 then
					man, exp = 0, 0
				else
					local exp1 = math.floor(math.log10(number:: number))
					man, exp = number/math.pow(10, exp1), exp1
				end
			end
		elseif types == 'table' then
			if #val >= 3 then 
				warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
				warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
				man, exp = val[1], val[2]
			end
			if #val == 2 then
				man, exp = val[1], val[2]
			elseif #val == 1 then
				if val[1] == 0 then
					man, exp = 0, 0
				else
					local exp1 = math.floor(math.log10(val[1]))
					man, exp = val[1]/math.pow(10, exp1), exp1
				end
			end
		end
		if i == 1 then
			bestMan, bestExp = man, exp
		else
			if exp > bestExp or (exp == bestExp and man > bestMan) then
				bestMan, bestExp = man, exp
			end
		end
	end
	return {bestMan, bestExp}
end

function Bnum.clamp(val: any, min: any, max: any): BN
	local vMan: number, vExp: number
	local loMan: number, loExp: number
	local hiMan: number, hiExp: number
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			vMan, vExp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			vMan, vExp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			vMan, vExp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			local exp1 = math.floor(math.log10(number:: number))
			vMan, vExp = number/math.pow(10, exp1), exp1
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			vMan, vExp = val[1], val[2]
		end
		if #val == 2 then
			vMan, vExp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				vMan, vExp = 0, 0
			else
				local exp = math.floor(math.log10(math.abs(val[1])))
				vMan, vExp = val[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(min)
	if t == 'number' then
		if min == 0 then
			loMan, loExp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			loMan, loExp = val/math.pow(10, exp1), exp1
		end
	elseif t == 'string' then
		local e = string.find(min, 'e')
		if e then
			loMan, loExp = tonumber(string.sub(min, 1, e-1)):: number, tonumber(string.sub(min, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(min)):: number
			local exp1 = math.floor(math.log10(number:: number))
			loMan, loExp = number/math.pow(10, exp1), exp1
		end
	elseif t == 'table' then
		if #min >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', min, 'to', {min[1], min[2]},' to BN\n    ', 'which is from',Bnum.toStr({min[1], min[2]}), 'to:', {min[1], min[2]})
			loMan, loExp = min[1], min[2]
		end
		if #min == 2 then
			loMan, loExp = min[1], min[2]
		elseif #min == 1 then
			if min[1] == 0 then
				loMan, loExp = 0, 0
			else
				local exp = math.floor(math.log10(math.abs(min[1])))
				loMan, loExp = min[1]/math.pow(10, exp), exp
			end
		end
	end
	local ty = type(max)
	if t == 'number' then
		if max == 0 then
			hiMan, hiExp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(max)))
			hiMan, hiExp = max/math.pow(10, exp1), exp1
		end
	elseif ty == 'string' then
		local e = string.find(max, 'e')
		if e then
			hiMan, hiExp = tonumber(string.sub(max, 1, e-1)):: number, tonumber(string.sub(max, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(max)):: number
			local exp1 = math.floor(math.log10(number:: number))
			hiMan, hiExp = number/math.pow(10, exp1), exp1
		end
	elseif ty == 'table' then
		if #max >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', max, 'to', {max[1], max[2]},' to BN\n    ', 'which is from',Bnum.toStr({max[1], max[2]}), 'to:', {max[1], max[2]})
			hiMan, hiExp = max[1], max[2]
		end
		if #max == 2 then
			hiMan, hiExp = max[1], max[2]
		elseif #max == 1 then
			local exp = math.floor(math.log10(math.abs(max[1])))
			hiMan, hiExp = max[1]/math.pow(10, exp), exp
		end
	end
	if loExp > hiExp or (loExp == hiExp and loMan > hiMan) then
		loMan, loExp, hiMan, hiExp = hiMan, hiExp, loMan, loExp
	end
	if vExp < loExp or (vExp == loExp and vMan < loMan) then
		return {loMan, loExp}
	end
	if vExp > hiExp or (vExp == hiExp and vMan > hiMan) then
		return {hiMan, hiExp}
	end
	return {vMan, vExp}
end

function Bnum.exp(val: any): BN
	local man, exp
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man, exp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			man, exp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(number:: number))
				man, exp = number/math.pow(10, exp1), exp1
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man, exp = val[1], val[2]
		end
		if #val == 2 then
			man, exp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(math.abs(val[1])))
				man, exp = val[1]/math.pow(10, exp1), exp1
			end
		end
	end
	if man == 0 then return {1, 0} end
	if exp == math.huge then return {1, math.huge} end
	if exp ~= exp then return {0/0, 0} end
	local pow = (man*math.pow(10, exp)) * 0.4342944819032518
	local e = math.floor(pow)
	return {math.pow(10, pow-e), exp+e}
end

function Bnum.random(val1: any?, val2: any?): BN
	val1 = val1 or '0e0'
	val2 = val2 or '1e0'
	local range = Bnum.sub(val2, val1)
	local factor = math.random()
	return Bnum.add(Bnum.mul(range, factor), val1)
end

function Bnum.lbencode(val: any): number
	local man, exp
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man, exp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			man, exp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(number:: number))
				man, exp = number/math.pow(10, exp1), exp1
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man, exp = val[1], val[2]
		end
		if #val == 2 then
			man, exp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(math.abs(val[1])))
				man, exp = val[1]/math.pow(10, exp1), exp1
			end
		end
	end
	if man == 0 then return 4e18 end
	local sign = man < 0 and -1 or 1
	man = math.abs(man)
	local expLog = math.log10(exp + 1)
	local expInt = math.floor(expLog * 1e6 + 0.5)
	local manLog = math.log10(man)
	local manInt = math.floor(manLog * 1e6 + 0.5)
	return sign * (expInt * 1e6 + manInt)
end

function Bnum.lbdecode(val: number): BN
	if val == 4e18 then return {0, 0} end
	local sign = math.sign(val)
	val = math.abs(val)
	local expPart = math.floor(val/1e6)
	local manPart = val % 1e6
	local exp = math.floor((math.pow(10, expPart/1e6)-1)+0.001)
	return {math.pow(10, manPart/1e6)*sign, exp}
end

function Bnum.encodeData(val: any, oldData: any): number
	local man, exp
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man, exp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			man, exp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(number:: number))
				man, exp = number/math.pow(10, exp1), exp1
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man, exp = val[1], val[2]
		end
		if #val == 2 then
			man, exp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(math.abs(val[1])))
				man, exp = val[1]/math.pow(10, exp1), exp1
			end
		end
	end
	if not oldData then
		if man == 0 then return 4e18 end
		local sign = man < 0 and -1 or 1
		man = math.abs(man)
		local expLog = math.log10(exp + 1)
		local expInt = math.floor(expLog * 1e6 + 0.5)
		local manLog = math.log10(man)
		local manInt = math.floor(manLog * 1e6 + 0.5)
		return sign * (expInt * 1e6 + manInt)
	end
	local new = {man, exp}
	if oldData then
		local old = Bnum.lbdecode(oldData)
		new = Bnum.max(old, new)
	end
	return Bnum.lbencode(new)
end

function Bnum.floor(val: any): BN
	local man, exp
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man, exp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			man, exp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(number:: number))
				man, exp = number/math.pow(10, exp1), exp1
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man, exp = val[1], val[2]
		end
		if #val == 2 then
			man, exp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(math.abs(val[1])))
				man, exp = val[1]/math.pow(10, exp1), exp1
			end
		end
	end
	if (man == 0 and exp == math.huge) or man ~= man or exp ~= exp then
		return {man, exp}
	end
	if exp > 0 then
		man = math.floor(man)
		if man == 0 then return {0, 0} end
		return {man, exp}
	end
	local f = math.floor(man * math.pow(10, exp))
	if f == 0 then return {0, 0} end
	local newE = math.floor(math.log10(math.abs(f)))
	return {f / math.pow(10, newE), newE}
end

function Bnum.mod(val1: any, val2: any): BN
	local man1: number, exp1: number = 0, 0
	local man2: number, exp2: number = 0, 0
	local types = type(val1)
	if types == 'number' then
		if val1 == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(val1))
			man1, exp1 = val1/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val1, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val1, 1, e-1)):: number, tonumber(string.sub(val1, e+1, -1)):: number
		else
			local number: number = tonumber(val1):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val1 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val1, 'to', {val1[1], val1[2]},' to BN\n    ', 'which is from',Bnum.toStr({val1[1],val1[2]}), 'to:', {val1[1], val1[2]})
			man1, exp1 = val1[1], val1[2]
		end
		if #val1 == 2 then
			man1, exp1 = val1[1], val1[2]
		elseif #val1 == 1 then
			if val1[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val1[1]))
				man1, exp1 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(val2)
	if t == 'number' then
		if val2 == 0 then
			man2, exp2 = 0,0
		else
			local exp = math.floor(math.log10(val2))
			man2, exp2 = val2/math.pow(10, exp), exp
		end
	elseif t == 'string' then
		local e = string.find(val2, 'e')
		if e then
			man2, exp2 = tonumber(string.sub(val2, 1, e-1)):: number, tonumber(string.sub(val2, e+1, -1)):: number
		else
			local number: number = tonumber(val2):: number
			if number == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man2, exp2 = number/math.pow(10, exp), exp
			end
		end
	elseif t == 'table' then
		if #val2 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val2, 'to', {val2[1], val2[2]},' to BN\n    ', 'which is from',Bnum.toStr({val2[1], val2[2]}), 'to:', {val2[1], val2[2]})
			man2, exp2 = val2[1], val2[2]
		end
		if #val2 == 2 then
			man2, exp2 = val2[1], val2[2]
		elseif #val2 == 1 then
			if val2[1] == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(val2[1]))
				man2, exp2 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	if man2 == 0 then return {0, 0} end
	if exp1 < exp2 or (exp1 == 0 and man1 < man2) then return {man1, exp1} end
	if exp1 == exp2 and man1 == man2 then return {0, 0} end
	local diff = exp1 - exp2
	if diff <= 15 then
		local n1 = man1 * 10^diff
		local ratio = n1 % man2
		if ratio == 0 then return {0, 0} end
		local shift = math.floor(math.log10(ratio))
		return {ratio / 10^shift, exp2 + shift}
	end
	local logA = math.log10(man1) + exp1
	local logB = math.log10(man2) + exp2
	local ratio = logA - logB
	local int = math.floor(ratio)
	local frac = ratio - int
	local rMan = math.pow(10, frac) * man2
	local rExp = exp2 + int
	local shift = math.floor(math.log10(rMan))
	return {rMan / math.pow(10, shift), rExp + shift}
end

function Bnum.modf(val: any): (BN, BN)
	local man, exp
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man, exp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			man, exp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(number:: number))
				man, exp = number/math.pow(10, exp1), exp1
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man, exp = val[1], val[2]
		end
		if #val == 2 then
			man, exp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(math.abs(val[1])))
				man, exp = val[1]/math.pow(10, exp1), exp1
			end
		end
	end
	if man == 0 or exp == math.huge or exp ~= exp then
		return {man, exp}, {0, 0}
	end
	if exp >= 0 then
		local intMan = man
		local intExp = exp
		return {intMan, intExp}, {0, 0}
	end
	return {0, 0}, {man, exp}
end

function Bnum.fmod(val1: any, val2: any): BN
	local man1: number, exp1: number = 0, 0
	local man2: number, exp2: number = 0, 0
	local types = type(val1)
	if types == 'number' then
		if val1 == 0 then
			man1, exp1 = 0, 0
		else
			local exp = math.floor(math.log10(val1))
			man1, exp1 = val1/math.pow(10, exp), exp
		end
	elseif types == 'string' then
		local e = string.find(val1, 'e')
		if e then
			man1, exp1 = tonumber(string.sub(val1, 1, e-1)):: number, tonumber(string.sub(val1, e+1, -1)):: number
		else
			local number: number = tonumber(val1):: number
			if number == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man1, exp1 = number/math.pow(10, exp), exp
			end
		end
	elseif types == 'table' then
		if #val1 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val1, 'to', {val1[1], val1[2]},' to BN\n    ', 'which is from',Bnum.toStr({val1[1],val1[2]}), 'to:', {val1[1], val1[2]})
			man1, exp1 = val1[1], val1[2]
		end
		if #val1 == 2 then
			man1, exp1 = val1[1], val1[2]
		elseif #val1 == 1 then
			if val1[1] == 0 then
				man1, exp1 = 0, 0
			else
				local exp = math.floor(math.log10(val1[1]))
				man1, exp1 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	local t = type(val2)
	if t == 'number' then
		if val2 == 0 then
			man2, exp2 = 0,0
		else
			local exp = math.floor(math.log10(val2))
			man2, exp2 = val2/math.pow(10, exp), exp
		end
	elseif t == 'string' then
		local e = string.find(val2, 'e')
		if e then
			man2, exp2 = tonumber(string.sub(val2, 1, e-1)):: number, tonumber(string.sub(val2, e+1, -1)):: number
		else
			local number: number = tonumber(val2):: number
			if number == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(number:: number))
				man2, exp2 = number/math.pow(10, exp), exp
			end
		end
	elseif t == 'table' then
		if #val2 >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val2, 'to', {val2[1], val2[2]},' to BN\n    ', 'which is from',Bnum.toStr({val2[1], val2[2]}), 'to:', {val2[1], val2[2]})
			man2, exp2 = val2[1], val2[2]
		end
		if #val2 == 2 then
			man2, exp2 = val2[1], val2[2]
		elseif #val2 == 1 then
			if val2[1] == 0 then
				man2, exp2 = 0, 0
			else
				local exp = math.floor(math.log10(val2[1]))
				man2, exp2 = val1[1]/math.pow(10, exp), exp
			end
		end
	end
	if man2 == 0 then return {0/0, 0} end
	if man1 == 0 then return {0, 0} end
	if exp1 < exp2 or (exp1 == exp2 and man1 < man2) then
		return {man1, exp1}
	end
	if exp1 == exp2 and man1 == man2 then
		return {0, 0}
	end
	local diff = exp1 - exp2
	if diff <= 15 then
		local n1 = man1 * 10^diff
		local ratio = n1 % man2
		if ratio == 0 then return {0, 0} end
		local shift = math.floor(math.log10(ratio))
		return {ratio / 10^shift, exp2 + shift}
	end
	local logA = math.log10(man1) + exp1
	local logB = math.log10(man2) + exp2
	local ratio = logA - logB
	local int = math.floor(ratio)
	local frac = ratio - int
	if frac == 0 then return {0, 0} end
	local rMan = 10^frac * man2
	local rExp = exp2 + int
	local shift = math.floor(math.log10(rMan))
	return {rMan / 10^shift, rExp + shift}
end

function Bnum.ceil(val: any): BN
	local man, exp
	local types = type(val)
	if types == 'number' then
		if val == 0 then
			man, exp = 0, 0
		else
			local exp1 = math.floor(math.log10(math.abs(val)))
			man, exp = val/math.pow(10, exp1), exp1
		end
	elseif types == 'string' then
		local e = string.find(val, 'e')
		if e then
			man, exp = tonumber(string.sub(val, 1, e-1)):: number, tonumber(string.sub(val, e+1, -1)):: number
		else
			local number: number = tonumber(math.abs(val)):: number
			if number == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(number:: number))
				man, exp = number/math.pow(10, exp1), exp1
			end
		end
	elseif types == 'table' then
		if #val >= 3 then 
			warn(`Failed to convert to BN cant go over 2 numbers in a table like {'{1, 2, 3}'}`)
			warn('AutoCorrected ', val, 'to', {val[1], val[2]},' to BN\n    ', 'which is from',Bnum.toStr({val[1], val[2]}), 'to:', {val[1], val[2]})
			man, exp = val[1], val[2]
		end
		if #val == 2 then
			man, exp = val[1], val[2]
		elseif #val == 1 then
			if val[1] == 0 then
				man, exp = 0, 0
			else
				local exp1 = math.floor(math.log10(val[1]))
				man, exp = val[1]/math.pow(10, exp1), exp1
			end
		end
	end
	if exp > 0 then
		local int = math.floor(man*math.pow(10, exp))
		if man*math.pow(10, exp) > int then
			int += 1
		end
		local newE = math.floor(math.log10(int))
		return {int/math.pow(10, exp), exp}
	end
	local shift = -exp
	local m = man * math.pow(10, shift)
	return {math.ceil(m), exp - shift}
end

function Bnum.maxBuy(val1: any, val2: any, multi: any): (BN, BN)
	local min = Bnum.sub(multi, 1)
	local currMul = Bnum.mul(val1, min)
	local currdiv = Bnum.div(currMul, val2)
	local inLog = Bnum.add(currdiv, 1)
	local totalAmount = Bnum.floor(Bnum.log(inLog, multi))
	local multiPow = Bnum.pow(multi, totalAmount)
	local multiPowSub = Bnum.sub(multiPow, 1)
	local totalDiv = Bnum.div(multiPowSub, min)
	local totalCost = Bnum.mul(totalDiv, val2)
	return totalAmount, totalCost
end

return table.freeze(Bnum)
