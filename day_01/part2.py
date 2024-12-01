def solve(data):
    lines = data.split("\n")
    left_list = []
    right_list = []
    for line in lines:
        numbers = line.split("   ")
        left_list.append(int(numbers[0]))
        right_list.append(int(numbers[-1]))

    dict = {}

    for number in left_list:
        dict[number] = 0

    for number in right_list:
        if number in dict:
            dict[number] += 1 

    res = 0
    for number in left_list:
        res += number * dict[number]

    return res

if __name__ == "__main__":
    data = ""
    with open("day_01/part2.txt", "r") as f:
        data = f.read()
    res = solve(data)

    print(res)