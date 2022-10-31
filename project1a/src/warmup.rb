def fib(n)
    a = Array.new
    x = 0
    y = 1
    step = 0
    while step < n do
	a.push(x)
	
	z = x + y
	x = y
	y = z
	step = step + 1
    end
    return a
end

def isPalindrome(n)
	
    if n.to_s == n.to_s.reverse then
        return true
    else
        return false
    end
end

def nthmax(n, a)
    s = nil
    if n > a.length() then
	s = nil
    else
	a.sort!
	s= a[a.length - n -1];
    end
return s
end

def freq(s)
    fr = Hash.new
    if s.empty? then
        return ""
    else
        s.split('').each { |c| 
            if not fr.has_key?(c)
                fr[c] = s.count(c)
            end
        }
    end
    return fr.key(fr.values.max)
end

def zipHash(arr1, arr2)
  if arr1.length != arr2.length then
        return nil
    else
        
        return Hash[arr1.zip(arr2)]
    end 
end

def hashToArray(hash)
   m = []
    m = hash.to_a
    return m 
end
