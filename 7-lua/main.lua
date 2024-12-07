local function gaussian_elimination(matrix)
    local n = #matrix

    for i = 1, n do
        local pivot = matrix[i][i]
        if pivot == 0 then
            return nil, "No unique solution (singular matrix)"
        end
        for j = i, n + 1 do
            matrix[i][j] = matrix[i][j] / pivot
        end

        for k = i + 1, n do
            local factor = matrix[k][i]
            for j = i, n + 1 do
                matrix[k][j] = matrix[k][j] - factor * matrix[i][j]
            end
        end
    end


    local solution = {}
    for i = n, 1, -1 do
        solution[i] = matrix[i][n + 1]
        for j = i + 1, n do
            solution[i] = solution[i] - matrix[i][j] * solution[j]
        end
    end

    return solution
end


local augmented_matrix = {
    {2,  1, -1,  8},
    {-3, -1,  2, -11},
    {-2,  1,  2, -3}
}


local solution, err = gaussian_elimination(augmented_matrix)

if solution then
    print("Solution:")
    for i, x in ipairs(solution) do
        print(string.format("x%d = %.2f", i, x))
    end
else
    print("Error:", err)
end