# TODO: Use the symmetry to increase computation speed. It would be easiest to
# use a quarter of the matrix and then at the end mirror and rotate to build the
# result. I could also use an eight of the whole matrix but that would be more
# complicated. Don't forget to profile before and after. Also save some
# sandpiles before as rds to compare them with the new ones.
# TODO: Preliminary tests show that keeping everything as integers will help
# with speed. Just not very much. But it may matter with bigger sandpiles. Just
# make sure not to force integers when the center cell is > 2e9 because that's
# the limit an int can be in R. Also: which(pile > 3) -> which(pile > 3L)
# TODO: Add an option to use 8 neighbors per cell. Remember that this changes
# the limit n to 8. Adjust the documentation accordingly. The color vector must
# be of length 8. Can I use the same symmetry speed improvements as with n = 4?
# TODO: Iterate over an eight of the matrix for more speed. This is low priority
# so do the other improvements first.

#' Sandpiles
#'
#' Drop some sand in the center and see it topple.
#'
#' @param n The number of grains dropped in the center.
#' @param colors A vector of length 4 or \code{NULL}. Specifies the colors to be
#'   allocated to the values \code{0:4}. If \code{NULL} the number of grains per
#'   cell are returned instead.
#'
#' @return A matrix, either containing colors or the number of grains per
#'   cell, depending on the argument \code{colors}.
#'
#' @references \url{https://en.wikipedia.org/wiki/Abelian_sandpile_model}
#'
#' @examples
#' s <- sandpile(100)
#' plot(s)
#'
#' @export
sandpile <- function(n, colors = c("white", "lightgray", "darkgray", "black")) {
    stopifnot(
        n > 0
    )
    sidelength <- 11  # must be an odd number
    pile <- matrix(0, nrow = sidelength, ncol = sidelength)
    center <- ceiling(sidelength / 2)
    pile[center, center] <- n
    increase_by <- 10  # must be an even number
    to_topple <- which(pile > 3)
    while (length(to_topple) > 0) {
        if (any(to_topple <= sidelength)) {
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

    if (is.null(colors)) {
        return(pile)
    }

    pile <- matrix(colors[pile + 1], nrow = nrow(pile))

    class(pile) <- c("color_matrix", class(pile))
    pile
}
