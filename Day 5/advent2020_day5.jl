using(DelimitedFiles)

input = DelimitedFiles.readdlm("Day 5/input_day5.txt")

# part 1



function passport(x)

parsed_binary = reduce(replace, ["B"=>1, "F"=>0, "L"=>0, "R"=>1], init=x)
row = parse(Int, parsed_binary[1:7], base = 2)
column = parse(Int, parsed_binary[8:10], base = 2)
seatid = row * 8 + column
return seatid

end

@time begin

seatids = passport.(input) #using . to vectorize bc julia is op

maximum(seatids)

end

# part 2

@time begin

seat_range = minimum(seatids):maximum(seatids)

seat_diff = setdiff(seat_range, seatids)

end
