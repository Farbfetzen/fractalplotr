# TODO:
# - Use the symmetry to increase computation speed.
# - Add an option to use 8 neighbors per cell. Remember that this changes the
#   limit height to 8.


#' @export
sandpile <- function(height,
                     colors = c(1, 2/3, 1/3, 0),
                     return_colors = TRUE) {
    s <- sandpile_iterate(height)
    if (!return_colors) {
        return(invisible(s))
    }
    s <- color_sandpile(s, colors)
    class(s) <- c(class(s), "color_matrix")
    invisible(s)
}


sandpile_iterate <- function(height) {
    sidelength <- 11  # must be an odd number
    n <- sidelength ^ 2
    pile <- matrix(0, nrow = sidelength, ncol = sidelength)
    center <- ceiling(sidelength / 2)
    pile[center, center] <- height
    left_border <- seq(1, sidelength)
    increase_by <- 10  # must be an even number

    repeat {
        to_topple <- which(pile > 3)
        if (length(to_topple) == 0) break

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
            n <- sidelength ^ 2
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
    }
    pile
}


color_sandpile <- function(pile, colors) {
    pile <- pile + 1
    matrix(colors[pile], nrow = nrow(pile))
}
