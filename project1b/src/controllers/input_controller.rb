require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise

def read_ships_file(path)
    game = GameBoard.new 10, 10
    if read_file_lines(path) == false then
        return nil
    end
        
    ships_add = 0
   read_file_lines(path){ | line |
        
            if line =~ /\(([0-9]|10),([0-9]|10)\),\s(Up|Down|Left|Right),\s([1-5])$/ then
                shipPos = Position.new($1, $2)
                newship = Ship.new(shipPos, $3, $4)
                    if game.add_ship(newship) == true then
                        
                        ships_add=ships_add + 1
                    end
                    if ships_add == 5 then
                        return game
                    end
        
        end
        }
        return nil 
end
# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
    if read_file_lines(path) == false then
        return nil
    end
ar = Array.new
    read_file_lines(path){ |line|
        if line =~ /\(([0-9]|10),([0-9]|10)\)$/ then
            shi = Position.new($1, $2)
            ar.append(shi)
        end

    }
    return ar
end



# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end


