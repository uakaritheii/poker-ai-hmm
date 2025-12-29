results = {}

with open("winrate.txt", "r") as f:
    for line in f:
        line = line.strip()
        if "takes all" not in line:
            continue
        winner = line.split(" takes all")[0].split()[-1]
        if winner not in results:
            results[winner] = 1
        else:
            results[winner] += 1

total = sum(results.values())

print("Win Counts")
for player in results:
    print(f"{player}: {results[player]}")

print(f"\nTotal games: {total}\n")

print("Win Rate")
for player in results:
    rate = results[player] / total * 100
    print(f"{player}: {rate:.2f}%")
