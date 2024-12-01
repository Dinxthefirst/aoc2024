def solve(data):
    lines = data.split("\n")
    left_list = []
    right_list = []
    for line in lines:
        numbers = line.split("   ")
        left_list.append(int(numbers[0]))
        right_list.append(int(numbers[-1]))

    left_list.sort()
    right_list.sort()

    sum = 0
    for elem1, elem2 in zip(left_list, right_list):
        sum += elem1 - elem2 if elem1 > elem2 else elem2 - elem1

    return sum

if __name__ == "__main__":
    data = ""
    with open("day_01/input1.txt", "r") as f:
        data = f.read()
    res = solve(data)

    print(res)