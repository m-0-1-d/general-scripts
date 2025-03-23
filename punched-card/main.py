import time
import argparse

from alphabet import alphabet


def decode(path):
    """
    Decode a punched card form a file.
    """
    try:
        # read every line in file by path
        with open(path, 'r') as file:
            lines = file.readlines()

        # create a matrix from the lines, where each element is a character
        matrix = []
        for line in lines:
            matrix.append(list(line.strip()))

        # get the number of rows and columns
        num_rows = len(matrix)
        num_columns = len(matrix[0])

        # iterate over the columns and rows to get the key from alphabet
        message = ""
        for column in range(num_columns):
            key = ""
            for row in range(num_rows):
                if matrix[row][column] == "#":
                    key += str(row) + "_"
            # take character of message by key from alphabet
            message += alphabet[key]

        # return decoded message
        return message

    # if error, return the exception
    except Exception as exception:
        return f"An error occurred while card decoding: {exception}."


def encode(message):
    """
    Encode a message to a punched card and return path of the card.
    """
    try:
        # check if the message is valid
        MIN_LENGTH = 10
        MAX_LENGTH = 42
        COLUMN_LENGTH = 12
        
        if len(message) < MIN_LENGTH or len(message) > MAX_LENGTH:
            return f"Message length must be between {MIN_LENGTH} and {MAX_LENGTH} characters."

        # create a matrix with the message, where each element is array with a column characters
        message = message.upper()
        
        columns = []
        for character in message:
            for key, value in alphabet.items():
                if value == character:
                    indexes = key.split("_")[0:-1]
                    column = ["_"] * COLUMN_LENGTH
                    for index in indexes:
                        column[int(index)] = "#"
                    columns.append(column)

        # create a line for future file by the columns
        rows = []
        num_columns = len(columns)
        for row in range(COLUMN_LENGTH):
            line = ""
            for column in range(num_columns):
                line += columns[column][row]
            rows.append(line)

        # save the card in a file
        milliseconds = int(time.time() * 1000)
        path = f"./cards/{milliseconds}_card.txt"
        with open(path, 'w') as file:
            for row in rows:
                file.write(row + "\n")

        # return the path of the card
        return f"Success: {path}."

    # if error, return the exception
    except Exception as exception:
        return f"An error occurred while card creation: {exception}."


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="A script that decode and encode punched card.")
    parser.add_argument("-m", "--message", type=str, help="Example message.")
    parser.add_argument("-p", "--path", type=str, help="./example/path.txt")
    args = parser.parse_args()

    if args.message:
        print(encode(args.message))
    elif args.path:
        print(decode(args.path))
    else:
        print("No valid flag provided.")
