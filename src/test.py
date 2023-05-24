def move_up(array):
    # Make a copy of the original array
    original_array = [row[:] for row in array]

    # Transpose the array to simulate moving up
    transposed_array = [list(column) for column in zip(*array)]

    # Flag to keep track if a merge is possible
    merge_possible = False

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
                merge_possible = True

        # Move non-zero values to the top again
        new_column = [value for value in new_column if value != 0]
        new_column += [0] * (len(column) - len(new_column))

        # Update the column in the transposed array
        column[:] = new_column

    # Transpose the array back to its original shape
    array[:] = [list(row) for row in zip(*transposed_array)]

    # Check if a merge is possible and if any changes occurred during the move
    if merge_possible and array != original_array:
        return True
    else:
        return False

# Create a 4x4 two-dimensional array with 0 values
array = [
    [0, 2, 0, 0],
    [0, 0, 0, 0],
    [0, 2, 0, 0],
    [0, 0, 0, 0],
]

valid_move = move_up(array)

if valid_move:
    print("Valid move!")
else:
    print("Invalid move!")



# Print the array
for row in array:
    print(row)
