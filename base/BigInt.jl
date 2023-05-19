# BigInteger Class

module BigInteger

using Primes

export BigInteger

mutable struct BigInteger
    str::String
end

# Helper Functions

function trim(str::String)
    # Implement the trim function logic here
end


function add(str1::String, str2::String)::String
    str1_len = length(str1)
    str2_len = length(str2)
    sum = ""

    if str1_len == 0 && str2_len == 0
        sum = "0"
    elseif str1[1] == '-' && str2[1] == '-'
        if str1_len == 1 && str2_len == 1
            sum = "0"
        else
            sum = "-" * add(substr(str1, 2), substr(str2, 2))
        end
    elseif str1[1] == '-'
        sum = subtract(str2, substr(str1, 2))
    elseif str2[1] == '-'
        sum = subtract(str1, substr(str2, 2))
    else
        i = str1_len
        j = str2_len
        track_sum = 0
        carry = 0

        while i > 0 && j > 0
            track_sum = parse(Int, str1[i]) - 48 + parse(Int, str2[j]) - 48 + carry
            carry = div(track_sum, 10)
            sum = string(track_sum % 10) * sum
            i -= 1
            j -= 1
        end

        if i > 0 && j < 0
            while i > 0
                track_sum = parse(Int, str1[i]) - 48 + carry
                carry = div(track_sum, 10)
                sum = string(track_sum % 10) * sum
                i -= 1
            end
        elseif j > 0 && i < 0
            while j > 0
                track_sum = parse(Int, str2[j]) - 48 + carry
                carry = div(track_sum, 10)
                sum = string(track_sum % 10) * sum
                j -= 1
            end
        end

        if carry != 0
            sum = string(carry) * sum
        end
    end

    return trim(sum)
end

function subtract(str1::String, str2::String)
    str1_len = length(str1)
    str2_len = length(str2)
    sum = ""
    
    if str1 == str2
        return "0"
    elseif str1[1] == '-' && str2[1] == '-'
        if str2_len == 1 && str1_len == 1
            sum = "0"
        else
            t1 = bigint(substring(str1, 2))
            t2 = bigint(substring(str2, 2))
            temp = subtract(t2.str, t1.str)
            mx = maximum(t2.str, t1.str)
            if temp[1] != '-' && mx.str == t1.str
                sum = "-" * temp
            else
                sum = temp
            end
        end
    elseif str1[1] == '-'
        sum = "-" * add(substring(str1, 2), str2)
    elseif str2[1] == '-'
        sum = add(str1, substring(str2, 2))
    else
        # Implement Subtract

        i = str1_len
        j = str2_len
        track_sum = 0
        carry = 0
      
        if str1_len < str2_len
            tp = str1
            str1 = str2
            str2 = tp
            sum = "-" * subtract(str1, str2)
            return trim(sum)
        elseif str1_len == str2_len
            a = bigint(str1)
            b = bigint(str2)
            mx = maximum(a.str, b.str)
            if mx.str == str2
                tp = str1
                str1 = str2
                str2 = tp
                t1 = bigint(str1)
                t2 = bigint(str2)
                temp = subtract(str1, str2)
                sum = "-" * subtract(str1, str2)
                return trim(sum)
            end
        end

        val1 = 0
        val2 = 0

        for k in 1:str1_len
            i -= 1
            j -= 1

            if i >= 1
                val1 = parse(Int, str1[i])
            else
                val1 = 0
            end

            if j >= 1
                val2 = parse(Int, str2[j])
            else
                val2 = 0
            end

            track_sum = val1 - val2 - carry

            if track_sum < 0
                track_sum += 10
                carry = 1
            else
                carry = 0
            end

            sum = string(track_sum) * sum
        end
    end

    return trim(sum)
end


function multiply(str1::String, str2::String)
    toAddNeg = false
    str1_len = length(str1)
    str2_len = length(str2)
    ans = ""
    
    if str1[1] == '-' && str2[1] == '-'
        ans = multiply(substring(str1, 2), substring(str2, 2))
    elseif str1[1] == '-'
        toAddNeg = true
        ans = multiply(substring(str1, 2), str2)
    elseif str2[1] == '-'
        toAddNeg = true
        ans = multiply(str1, substring(str2, 2))
    else
        if str1_len == 0 || str2_len == 0
            return "0"
        end
        
        result = zeros(Int, str1_len + str2_len)
        i_n1 = 0
        i_n2 = 0
        
        for i = str1_len:-1:1
            carry = 0
            n1 = parse(Int, str1[i])
            i_n2 = 0
            
            for j = str2_len:-1:1
                n2 = parse(Int, str2[j])
                sum = n1 * n2 + result[i_n1 + i_n2] + carry
                carry = div(sum, 10)
                result[i_n1 + i_n2] = sum % 10
                i_n2 += 1
            end
            
            if carry > 0
                result[i_n1 + i_n2] += carry
            end
            
            i_n1 += 1
        end
        
        i = length(result)
        
        while i >= 1 && result[i] == 0
            i -= 1
        end
        
        if i == 0
            return "0"
        end
        
        while i >= 1
            ans *= string(result[i])
            i -= 1
        end
    end
    
    if toAddNeg && ans[1] != '0'
        ans = "-" * ans
    end
    
    return ans
end


function divide(str1::String, str2::String)
    ans = ""
    
    if str2 == "0"
        return "0"
    elseif str1 == str2
        return "1"
    elseif str1[1] == '-' && str2[1] == '-'
        ans = divide(str1[2:end], str2[2:end])
    elseif str1[1] == '-'
        temp = divide(str1[2:end], str2)
        if temp == "0"
            ans = temp
        else
            ans = "-" * temp
        end
    elseif str2[1] == '-'
        temp = divide(str1, str2[2:end])
        if temp == "0"
            ans = temp
        else
            ans = "-" * temp
        end
    else
        if str2 == "1"
            return str1
        end
        
        if is_strictlyMaximum(str2, str1)
            return "0"
        end
        
        if length(str2) <= 19
            int_str2 = parse(BigInt, str2)
            ans = shortDivide(str1, int_str2)
        else
            temp = str2
            ans = "0"
            count = "0"
            
            while str1 == maximum(str1, str2)
                lenDiff = length(str1) - length(str2)
                
                if lenDiff > 0 && str1[1] > str2[1]
                    count = add(count, pow("10", string(lenDiff)))
                    str1 = subtract(str1, multiply(str2, pow("10", string(lenDiff))))
                elseif lenDiff > 0
                    count = add(count, pow("10", string(lenDiff - 1)))
                    str1 = subtract(str1, multiply(str2, pow("10", string(lenDiff - 1))))
                else
                    count = add(count, "1")
                    str1 = subtract(str1, str2)
                end
            end
            
            ans = count
        end
    end
    
    return ans
end

function shortDivide(s1::String, divisor::UInt64)
    ans = ""
    idx = 1
    temp = parse(UInt64, s1[idx])
    
    while temp < divisor
        idx += 1
        temp = temp * 10 + parse(UInt64, s1[idx])
        
        if idx >= length(s1)
            break
        end
    end
    
    while length(s1) > idx
        ans *= string(div(temp, divisor))
        temp = (temp % divisor) * 10 + parse(UInt64, s1[idx + 1])
        idx += 1
    end
    
    if length(ans) == 0
        return "0"
    end
    
    return ans
end

function mod(str1::String, str2::String)
    ans = subtract(str1, multiply(divide(str1, str2), str2))
    return ans
end

function sqrt(s::String)
    if s[1] == '-'
        return s
    end
    if s == "0"
        return "0"
    end
    s_len = length(s)
    ans = ""
    mid = ""
    high = ""
    low = ""
    square = ""
    ans_len = div(s_len, 2)
    if isodd(s_len)
        low = pow("10", string(ans_len))
        high = pow("10", string(ans_len + 1))
    else
        low = pow("10", string(ans_len - 1))
        high = pow("10", string(ans_len))
    end
    prev = ""
    while true
        mid = divide(add(high, low), "2")
        square = multiply(mid, mid)
        if prev == mid || (maximum(add(square, mid), s) == add(square, mid) && maximum(square, s) == s) || high == low
            break
        end
        if maximum(square, s) == s
            low = mid
        elseif maximum(square, s) == square
            high = mid
        end
        prev = mid
    end
    ans = mid
    return ans
end

function log2(s::String)
    if s == "0"
        return string(log2(0))
    end
    if s[1] == '-'
        return string(log2(-1))
    end
    logVal = "-1"
    while s != "0"
        logVal = add(logVal, "1")
        s = divide(s, "2")
    end
    return logVal
end

function log10(s::String)
    if s == "0"
        return string(log2(0))
    end
    if s[1] == '-'
        return string(log2(-1))
    end
    return string(length(s) - 1)
end

function logwithbase(val::String, base::String)
    return divide(log2(val), log2(base))
end

function antilog2(s::String)
    return pow("2", s)
end

function antilog10(s::String)
    return pow("10", s)
end

function swap(str1::String, str2::String)
    str1, str2 = str2, str1
end

function reverse(s::String)
    fl = false
    if s[1] == '-'
        s = s[2:end]
        fl = true
    end
    beg = 1
    en = length(s)
    while beg < en
        ch = s[beg]
        s[beg] = s[en]
        s[en] = ch
        beg += 1
        en -= 1
    end
    if fl
        s = '-' * s
    end
    return s
end

function pow(str1::String, str2::String)
    if str2 == "0"
        return "1"
    elseif str1 == "0"
        if str2[1] == '-'
            return string(Base.Math.pow(0, -5)) #ChatGPT decided to use Base.Math.pow here on its own. ??!!
        end
        return "0"
    elseif str1[1] == '-' && str2[1] == '-'
        if str1 == "-1" && str2 == "-1"
            return "-1"
        elseif str1 == "-1"
            if (Int(str2[end]) - 48) & 1 != 0
                return "-1"
            else
                return "1"
            end
        else
            return "0"
        end
    elseif str1[1] == '-'
        if (Int(str2[end]) - 48) & 1 != 0
            return "-" * pow(str1[2:end], str2)
        end
        return pow(str1[2:end], str2)
    elseif str2[1] == '-'
        if str1 == "1"
            return str1
        else
            return "0"
        end
    else
        init_str1 = str1
        while str2 != "1"
            str1 = multiply(str1, init_str1)
            str2 = subtract(str2, "1")
        end
        return str1
    end
end

function gcd(str1::String, str2::String)
    if is_strictlyMaximum(str2, str1)
        swap(str1, str2)
    end
    temp = ""
    while is_strictlyMaximum(str2, "0")
        temp = mod(str1, str2)
        str1 = str2
        str2 = temp
    end
    return str1
end

function lcm(str1::String, str2::String)
    return divide(multiply(str1, str2), gcd(str1, str2))
end

function fact(s::String)
    if s[1] == '-'
        throw(DomainError("Factorial of a negative integer is not defined."))
    end
    if s == "0"
        return "1"
    end
    ans = "1"
    while s != "0"
        ans = multiply(ans, s)
        s = subtract(s, "1")
    end
    return ans
end

function isPalindrome(s::String)
    if s[1] == '-'
        s = s[2:end]
    end
    beg = 1
    ends = length(s)
    while beg < ends
        if s[beg] != s[ends]
            return false
        end
        beg += 1
        ends -= 1
    end
    return true
end

function isPrime(s::String)
    if maximum(s, "2") != s
        return false
    end
    sqrt = bigint.sqrt(s)
    i = "2"
    while is_maximum(sqrt, i)
        if mod(s, i) == "0"
            return false
        end
        i = add(i, "1")
    end
    return true
end


function maximum(str1::String, str2::String)
    max = ""
    bothNeg = false
    isMax1 = false
    isMax2 = false
    
    if str1[1] == '-' && str2[1] == '-'
        bothNeg = true
        str1 = str1[2:end]
        str2 = str2[2:end]
    elseif str1[1] == '-'
        return trim(str2)
    elseif str2[1] == '-'
        return trim(str


function is_bigint(s::String)
    if s[1] == '-'
        s = s[2:end]
    end
    
    for i in 1:length(s)
        if !(48 <= Int(s[i]) <= 57)
            return false
        end
    end
    
    return true
end


# Conversion Functions

function _to_bigint(s::String)
    return BigInt(s)
end

function _to_bigint(n::Int)
    return BigInt(string(n))
end

function _to_bigint(n::Int64)
    return BigInt(string(n))
end

function _to_bigint(n::Int128)
    return BigInt(string(n))
end

#External Functions:

function _big_max(a::BigInt, b::BigInt)
    return max(a, b)
end

function _big_min(a::BigInt, b::BigInt)
    return min(a, b)
end

function _big_abs(a::BigInt)
    return abs(a)
end

function _big_pow(a::BigInt, b::BigInt)
    return a^b
end

function _big_sqrt(a::BigInt)
    return isqrt(a)
end

function _big_log2(a::BigInt)
    return floor(log2(a))
end

function _big_log10(a::BigInt)
    return floor(log10(a))
end

function _big_logwithbase(a::BigInt, b::BigInt)
    return floor(log(a, b))
end

function _big_antilog2(a::BigInt)
    return BigInt(2)^a
end

function _big_antilog10(a::BigInt)
    return BigInt(10)^a
end

function _big_swap(a::BigInt, b::BigInt)
    return b, a
end

function _big_reverse(a::BigInt)
    return parse(BigInt, reverse(string(a)))
end

function _big_gcd(a::BigInt, b::BigInt)
    return gcd(a, b)
end

function _big_lcm(a::BigInt, b::BigInt)
    return lcm(a, b)
end

function _big_fact(a::BigInt)
    if a < 0
        throw(DomainError("Factorial of a negative integer is not defined."))
    end
    return factorial(a)
end

function _big_isPalindrome(a::BigInt)
    return ispalindrome(string(a))
end

function _big_isPrime(a::BigInt)
    return isprime(a)
end


# Operator Overloading

import Base: +, -, *, /, %, ==

function +(a::BigInteger, b::BigInteger)
    BigInteger(add(a.str, b.str))
end

function +(a::BigInteger, b::Int)
    BigInteger(add(a.str, string(b)))
end

function +(a::Int, b::BigInteger)
    BigInteger(add(string(a), b.str))
end

function -(a::BigInteger, b::BigInteger)
    BigInteger(subtract(a.str, b.str))
end

function -(a::BigInteger, b::Int)
    BigInteger(subtract(a.str, string(b)))
end

function -(a::Int, b::BigInteger)
    BigInteger(subtract(string(a), b.str))
end

function *(a::BigInteger, b::BigInteger)
    BigInteger(multiply(a.str, b.str))
end

function *(a::BigInteger, b::Int)
    BigInteger(multiply(a.str, string(b)))
end

function *(a::Int, b::BigInteger)
    BigInteger(multiply(string(a), b.str))
end

function /(a::BigInteger, b::BigInteger)
    BigInteger(divide(a.str, b.str))
end

function /(a::BigInteger, b::Int)
    BigInteger(divide(a.str, string(b)))
end

function /(a::Int, b::BigInteger)
    BigInteger(divide(string(a), b.str))
end

function %(a::BigInteger, b::BigInteger)
    BigInteger(mod(a.str, b.str))
end

function %(a::BigInteger, b::Int)
    BigInteger(mod(a.str, string(b)))
end

function %(a::Int, b::BigInteger)
    BigInteger(mod(string(a), b.str))
end

function ==(a::BigInteger, b::BigInteger)
    a.str == b.str
end

end  # module
