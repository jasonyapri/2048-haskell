def swipeUp(array):
    # Make a copy of the original array
    original_array = [row[:] for row in array]

    # Transpose the array to simulate moving up
    transposed_array = [list(column) for column in zip(*array)]

    # Flag to keep track if a merge is possible
    didnt_move = True

    # Perform the movement for each column
    for column in transposed_array:
        # Move non-zero values to the top
        new_column = [value for value in column if value != 0]
        new_column += [0] * (len(column) - len(new_column))

        # Merge adjacent equal values
        for i in range(len(new_column) - 1):
            if new_column[i] == new_column[i + 1]:
                new_column[i] *= 2
                new_column[i + 1] = 0
                didnt_move = False

        # Move non-zero values to the top again
        new_column = [value for value in new_column if value != 0]
        new_column += [0] * (len(column) - len(new_column))

        # Update the column in the transposed array
        column[:] = new_column

    # Transpose the array back to its original shape
    array[:] = [list(row) for row in zip(*transposed_array)]

    # Check if a merge is possible and if any changes occurred during the move
    if not didnt_move and array != original_array:
        return True
    else:
        return False

def swipeDown(array):
    # Make a copy of the original array
    original_array = [row[:] for row in array]

    # Reverse the array to simulate moving down
    reversed_array = [row[::-1] for row in array]

    # Transpose the reversed array to perform the movement
    transposed_array = [list(column) for column in zip(*reversed_array)]

    # Flag to keep track if a merge is possible
    didnt_move = True

    # Perform the movement for each column
    for column in transposed_array:
        # Move non-zero values to the bottom
        new_column = [value for value in column if value != 0]
        new_column = [0] * (len(column) - len(new_column)) + new_column

        # Merge adjacent equal values
        for i in range(len(new_column) - 1, 0, -1):
            if new_column[i] == new_column[i - 1]:
                new_column[i] *= 2
                new_column[i - 1] = 0
                didnt_move = False

        # Move non-zero values to the bottom again
        new_column = [value for value in new_column if value != 0]
        new_column = [0] * (len(column) - len(new_column)) + new_column

        # Update the column in the transposed array
        column[:] = new_column

    # Transpose the array back to its original shape
    transposed_array = [list(row) for row in zip(*transposed_array)]

    # Reverse the array again to restore the original order
    array[:] = [row[::-1] for row in transposed_array]

    # Check if a merge is possible and if any changes occurred during the move
    if not didnt_move and array != original_array:
        return True
    else:
        return False

# Create a 4x4 two-dimensional array with 0 values
array = [
    [0, 2, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 2, 0],
    [0, 2, 0, 0],
]

valid_move = swipeDown(array)

if valid_move:
    print("Can move!")
else:
    print("Cannot move!")

# Print the array
for row in array:
    print(row)
