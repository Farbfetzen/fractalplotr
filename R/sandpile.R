# TODO: Use the symmetry to increase computation speed. Don't forget to profile before and after.
# TODO: Add an option to use 8 neighbors per cell. Remember that this changes the
#   limit n to 8. Adjust the documentation accordingly. The color vector must be of length 8.


#' Sandpiles
#'
#' Drop some sand in the center and see it topple.
#'
#' @param n The number of grains dropped in the center.
#' @param colors A vector of length 4 containing the colors to be allocated to
#'   zero to three grains per cell.
#' @param return_colors Logical. Should the returned matrix consist of the
#'   numbers of grains per cell or of the corresponding colors?
#'
#' @return A matrix. Either with the number of grains per cell or the
#'   corresponding colors, depending on the value of \code{return_colors}.
#' @references \link{https://en.wikipedia.org/wiki/Abelian_sandpile_model}
#' @export
#'
#' @examples
#' sandpile(100, return_colors = FALSE)
sandpile <- function(n,
                     colors = c("white", "lightgray", "darkgray", "black"),
                     return_colors = TRUE) {
    stopifnot(
        n > 0
    )
    sidelength <- 11  # must be an odd number
    pile <- matrix(0, nrow = sidelength, ncol = sidelength)
    center <- ceiling(sidelength / 2)
    pile[center, center] <- n
    left_border <- seq_len(sidelength)
    increase_by <- 10  # must be an even number
    to_topple <- which(pile > 3)
    while (length(to_topple) > 0) {
        if (any(to_topple %in% left_border)) {
            # Increase the size of the pile
            new_sidelength <- sidelength + 2 * increase_by
            new_pile <- matrix(0, nrow = new_sidelength, ncol = new_sidelength)
            middle <- seq(increase_by + 1, length.out = sidelength)
            coords <- cbind(
                row = rep(middle, sidelength),
                col = rep(middle, each = sidelength)
            )
            new_pile[coords] <- pile
            pile <- new_pile
            sidelength <- new_sidelength
            left_border <- seq(1, sidelength)
            to_topple <- which(pile > 3)
        }

        pile[to_topple] <- pile[to_topple] - 4

        top_neighbors <- to_topple - 1
        bottom_neighbors <- to_topple + 1
        left_neighbors <- to_topple - sidelength
        right_neighbors <- to_topple + sidelength

        pile[top_neighbors] <- pile[top_neighbors] + 1
        pile[bottom_neighbors] <- pile[bottom_neighbors] + 1
        pile[left_neighbors] <- pile[left_neighbors] + 1
        pile[right_neighbors] <- pile[right_neighbors] + 1

        to_topple <- which(pile > 3)
    }

    # trim the edges solely consisting of zeroes:
    pile <- pile[rowSums(pile) > 0, colSums(pile) > 0, drop = FALSE]

    if (!return_colors) {
        return(pile)
    }

    pile <- matrix(colors[pile + 1], nrow = nrow(pile))

    class(pile) <- c("color_matrix", class(pile))
    pile
}
