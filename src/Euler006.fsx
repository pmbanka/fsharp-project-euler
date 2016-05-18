let squareOfSum n =
    let sum = n * (n + 1) / 2
    sum * sum

let sumOfSquares n =
    n * (n + 1) * (2*n + 1) / 6

squareOfSum 100 - sumOfSquares 100