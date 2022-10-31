class PhoneBook
    def initialize
        @phone_name = []
        @number = []
        @phone_bool = []
    end
    
    def display
        puts @phone_name
        puts @number
        puts @phone_bool
    end
    
    def is_digit?(s)
        code = s.ord
        48 <= code && code <= 57
    end

    def add(name, number, is_listed)
        n = 0
        k=0
        name1 = false
        number0 = false
        is_number_valid = false
@phone_name.each{ |na|
            if na.eql?(name) then
                return false
            end
        }
       if number.length == 12 && number[3] == '-' && number[7] == '-' then
           number.split('').each { |c|
            if c == '-' && n == 3 then
               n = 0
               k=k+1
            elsif is_digit?(c)
                n = n+1
else
return false
            end
           }
        else 
            return false
        end
        
    if k == 2 && n == 4 then
        is_number_valid = true;
else
return false
    end
    if is_number_valid == true then
        if @phone_name.length == 0 then
                    @phone_name.push(name)
                    @number.push(number)
                    @phone_bool.push(is_listed)
                    return true
        else
                if is_listed == false
                    @phone_name.each{ |n|
                        if !n.eql?(name) then
                            name1 = true
                        else
                            return false
                        end
                    }
                    if name1 == true then
                        @phone_name.push(name)
                        @number.push(number)
                        @phone_bool.push(is_listed)
                        return true
                    end
                elsif is_listed == true
                    @number.each{ |num|
                        if !num.eql?(number) then
                            number0 = true
                        elsif num.eql?(number) && 
                        @phone_bool[@number.index(num)] == false
                            number0 = true
                        else
                            return false
                        end
                    }
                    if number0 == true then
                        @phone_name.push(name)
                        @number.push(number)
                        @phone_bool.push(is_listed)
                        return true
                    end
        end#second if end
    end#first if end
    end #function end
end#class end

def lookup(name)
    if !@phone_name.include?(name)
        return nil
    end
        if @phone_bool[@phone_name.index(name)] == true
            return @number[@phone_name.index(name)]
        else
            return nil
end
end
def lookupByNum(number)
       if !@number.include?(number)
        return nil
    end
        if @phone_bool[@number.index(number)] == true
            return @phone_name[@number.index(number)]
        else
            return nil
    end
end

def namesByAc(areacode)
        area_array = []
        @number.each{ |n|
            if n[0..2].eql?(areacode) then
                area_array.push(@phone_name[@number.index(n)])
            end
        }
        return area_array
    end

end
