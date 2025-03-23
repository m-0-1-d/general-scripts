import argparse


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="A console application for playing games with bots.")
    parser.add_argument("-g", "--game", type=str, help="Game name: tictactoe, minesweeper, nim.")
    args = parser.parse_args()

    if args.game == "tictactoe":
        print("tictactoe")
    elif args.game == "minesweeper":
        print("minesweeper")
    elif args.game == "nim":
        print("nim")
    else:
        print("No game found.")