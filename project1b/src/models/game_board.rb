class GameBoard
    # @max_row is an `Integer`
    # @max_column is an `Integer`
    attr_reader :max_row, :max_column

    def initialize(max_row, max_column)
        @max_row = max_row
        @max_column = max_column
@array_grid = Array.new(max_row){Array.new(max_column,"-")}
        @num_hits = 0
    end
    def add_ship(ship)
    if ship.start_position.row.to_i > @max_row.to_i || ship.start_position.column.to_i > @max_row.to_i || ship.start_position.row.to_i < 1 || ship.start_position.column.to_i< 1
            return false
        end
        
        
        row = Integer(ship.start_position.row)
        col = Integer(ship.start_position.column)

        if row >= 1 or row <= @max_row or col >= 1 or col <= @max_column then
        if ship.orientation.eql?("Right") then
            if ship.size.to_i + ship.start_position.column.to_i - 1 > @max_column.to_i then
                return false
            end
        elsif ship.orientation.eql?("Left") then
            if ship.start_position.column.to_i-ship.size.to_i < 0 then
                return false
            end
        elsif ship.orientation.eql?("Up") then
            if ship.start_position.row.to_i-ship.size.to_i < 0 then
                return false
            end
        elsif ship.orientation.eql?("Down") then
            if ship.start_position.row.to_i-1 > @max_row.to_i then
                return false
            end
        end


        if ship.orientation.eql?("Right") then
if ship.size.to_i+ship.start_position.column.to_i-2 >= @max_row.to_i then
return false
end 
            for i in ship.start_position.column.to_i-1..ship.size.to_i+ship.start_position.column.to_i-2 do
                if @array_grid[ship.start_position.row.to_i-1][i] != "-" then
                    return false
                else
                    @array_grid[ship.start_position.row.to_i-1][i] = "A"
                end
            end
        elsif ship.orientation.eql?("Left") then
if (ship.start_position.column.to_i-1) < 1 then
return false
end
            for i in ship.start_position.column.to_i-ship.size.to_i..(ship.start_position.column.to_i-1) do
                if @array_grid[ship.start_position.row.to_i-1][i] != "-" then
                    return false
                else
                    @array_grid[ship.start_position.row.to_i-1][i] = "A"
                end
            end
        elsif ship.orientation.eql?("Up") then
if ship.start_position.row.to_i-1 < 1 then
return false
end
            for i in ship.start_position.row.to_i-ship.size.to_i..ship.start_position.row.to_i-1 do
                if @array_grid[i][ship.start_position.column.to_i-1] != "-" then
                    return false
                else
                    @array_grid[i][ship.start_position.column.to_i-1] = "A"
                end
            end
        elsif ship.orientation.eql?("Down") then
if ship.size.to_i+ship.start_position.row.to_i-2 >= @max_row.to_i then
return false
end
            for i in ship.start_position.row.to_i-1..ship.size.to_i+ship.start_position.row.to_i-2 do
                if @array_grid[i][ship.start_position.column.to_i-1] != "-" then
                    return false
                else
                    @array_grid[i][ship.start_position.column.to_i-1] = "A"
                end
            end
        end
        return true
    end
        return false    
    end
    

    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defined)
 def attack_pos(position)
        # check position
        if position.row.to_i > @max_row.to_i || position.column.to_i> @max_column.to_i || position.row.to_i < 1 || position.column.to_i < 1 then
            return nil
        end

        if @array_grid[position.row.to_i - 1][position.column.to_i - 1].eql?("A")
            @num_hits = @num_hits + 1
            @array_grid[position.row.to_i- 1][position.column.to_i - 1] = "B"
            return true
        elsif @array_grid[position.row.to_i - 1][position.column.to_i - 1].eql?("B")
            return true
        else
            return false
        end

        # update your grid

        # return whether the attack was successful or not
    end
    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return @num_hits
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
        @array_grid.each do |sub|
            sub.each do |str|
              if str.eql?("A") then
                return false
            end
          end
        end
          return true
    end


    # String representation of GameBoard (optional but recommended)
    def to_s
        puts @array_grid.map { |x| x.join(' ') }
    end
end
