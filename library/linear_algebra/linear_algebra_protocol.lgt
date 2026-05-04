:- protocol(linear_algebra_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'Linear algebra protocol.'
	]).

	:- public(new_vector/3).
	:- mode(new_vector(+integer, +number, -list(number)), one).
	:- info(new_vector/3, [
		comment is 'Constructs a numeric vector with the requested length, filled with the given value.',
		argnames is ['Count', 'Value', 'Vector']
	]).

	:- public(new_vector_like/2).
	:- mode(new_vector_like(+list, -list(number)), one).
	:- info(new_vector_like/2, [
		comment is 'Constructs a numeric zero vector matching the length of the reference list.',
		argnames is ['Reference', 'Zeroes']
	]).

	:- public(add_vectors/3).
	:- mode(add_vectors(+list(number), +list(number), -list(number)), one).
	:- info(add_vectors/3, [
		comment is 'Adds two numeric vectors element-wise.',
		argnames is ['Vector1', 'Vector2', 'Vector']
	]).

	:- public(subtract_vectors/3).
	:- mode(subtract_vectors(+list(number), +list(number), -list(number)), one).
	:- info(subtract_vectors/3, [
		comment is 'Subtracts the second numeric vector from the first element-wise.',
		argnames is ['Vector1', 'Vector2', 'Vector']
	]).

	:- public(add_scaled_vector/4).
	:- mode(add_scaled_vector(+list(number), +number, +list(number), -list(number)), one).
	:- info(add_scaled_vector/4, [
		comment is 'Adds a scaled numeric vector to another numeric vector.',
		argnames is ['Vector', 'Scale', 'Vector0', 'UpdatedVector']
	]).

	:- public(scale_vector/3).
	:- mode(scale_vector(+list(number), +number, -list(number)), one).
	:- info(scale_vector/3, [
		comment is 'Scales a numeric vector by the given factor.',
		argnames is ['Vector', 'Scale', 'ScaledVector']
	]).

	:- public(dot_product/3).
	:- mode(dot_product(+list(number), +list(number), -number), one).
	:- info(dot_product/3, [
		comment is 'Computes the dot product of two numeric vectors.',
		argnames is ['Vector1', 'Vector2', 'Product']
	]).

	:- public(euclidean_norm/2).
	:- mode(euclidean_norm(+list(number), -float), one).
	:- info(euclidean_norm/2, [
		comment is 'Computes the Euclidean norm of a numeric vector.',
		argnames is ['Vector', 'Norm']
	]).

	:- public(vector_norm/3).
	:- mode(vector_norm(+list(number), @term, -number), one_or_error).
	:- info(vector_norm/3, [
		comment is 'Computes the norm of a numeric vector for a positive numeric order or the atoms `inf` and `infinity`.',
		argnames is ['Vector', 'Order', 'Norm']
	]).

	:- public(basis_vector/3).
	:- mode(basis_vector(+integer, +integer, -list(number)), one).
	:- info(basis_vector/3, [
		comment is 'Constructs a numeric basis vector with value 1.0 at the given one-based index and 0.0 elsewhere.',
		argnames is ['Size', 'Index', 'Vector']
	]).

	:- public(new_matrix/4).
	:- mode(new_matrix(+integer, +integer, +number, -list(list(number))), one).
	:- info(new_matrix/4, [
		comment is 'Constructs a numeric matrix with the requested row and column counts, filled with the given value.',
		argnames is ['Rows', 'Columns', 'Value', 'Matrix']
	]).

	:- public(identity_matrix/2).
	:- mode(identity_matrix(+integer, -list(list(number))), one).
	:- info(identity_matrix/2, [
		comment is 'Constructs a square identity matrix with the requested size.',
		argnames is ['Size', 'Matrix']
	]).

	:- public(matrix_vector_product/3).
	:- mode(matrix_vector_product(+list(list(number)), +list(number), -list(number)), one).
	:- info(matrix_vector_product/3, [
		comment is 'Computes the matrix-vector product for a numeric matrix and vector, returning the list of row-wise dot products between each matrix row and the vector.',
		argnames is ['Matrix', 'Vector', 'Product']
	]).

	:- public(matrix_matrix_product/3).
	:- mode(matrix_matrix_product(+list(list(number)), +list(list(number)), -list(list(number))), one).
	:- info(matrix_matrix_product/3, [
		comment is 'Computes the matrix-matrix product for two numeric matrices.',
		argnames is ['Matrix1', 'Matrix2', 'Product']
	]).

	:- public(gram_matrix/2).
	:- mode(gram_matrix(+list(list(number)), -list(list(number))), one).
	:- info(gram_matrix/2, [
		comment is 'Computes the row Gram matrix of a numeric row matrix by taking all row-wise dot products.',
		argnames is ['Rows', 'GramMatrix']
	]).

	:- public(matrix_row_means/2).
	:- mode(matrix_row_means(+list(list(number)), -list(number)), one).
	:- info(matrix_row_means/2, [
		comment is 'Computes the arithmetic mean of each row in a numeric matrix.',
		argnames is ['Matrix', 'Means']
	]).

	:- public(matrix_column_means/2).
	:- mode(matrix_column_means(+list(list(number)), -list(number)), one).
	:- info(matrix_column_means/2, [
		comment is 'Computes the arithmetic mean of each column in a numeric matrix.',
		argnames is ['Matrix', 'Means']
	]).

	:- public(center_gram_matrix/4).
	:- mode(center_gram_matrix(+list(list(number)), -list(list(number)), -list(number), -number), one).
	:- info(center_gram_matrix/4, [
		comment is 'Double-centers a numeric Gram matrix and also returns its row means and total mean.',
		argnames is ['GramMatrix', 'CenteredGramMatrix', 'RowMeans', 'TotalMean']
	]).

	:- public(center_gram_vector/4).
	:- mode(center_gram_vector(+list(number), +list(number), +number, -list(number)), one).
	:- info(center_gram_vector/4, [
		comment is 'Centers a numeric Gram vector using training row means and total mean.',
		argnames is ['GramVector', 'RowMeans', 'TotalMean', 'CenteredGramVector']
	]).

	:- public(outer_product/3).
	:- mode(outer_product(+list(number), +list(number), -list(list(number))), one).
	:- info(outer_product/3, [
		comment is 'Computes the outer product of two numeric vectors.',
		argnames is ['Vector1', 'Vector2', 'Matrix']
	]).

	:- public(add_matrices/3).
	:- mode(add_matrices(+list(list(number)), +list(list(number)), -list(list(number))), one).
	:- info(add_matrices/3, [
		comment is 'Adds two numeric matrices element-wise.',
		argnames is ['Matrix1', 'Matrix2', 'Matrix']
	]).

	:- public(subtract_matrices/3).
	:- mode(subtract_matrices(+list(list(number)), +list(list(number)), -list(list(number))), one).
	:- info(subtract_matrices/3, [
		comment is 'Subtracts the second numeric matrix from the first element-wise.',
		argnames is ['Matrix1', 'Matrix2', 'Matrix']
	]).

	:- public(scale_matrix/3).
	:- mode(scale_matrix(+list(list(number)), +number, -list(list(number))), one).
	:- info(scale_matrix/3, [
		comment is 'Scales each element of a numeric matrix by the given factor.',
		argnames is ['Matrix', 'Scale', 'ScaledMatrix']
	]).

	:- public(frobenius_norm/2).
	:- mode(frobenius_norm(+list(list(number)), -float), one_or_error).
	:- info(frobenius_norm/2, [
		comment is 'Computes the Frobenius norm of a numeric matrix.',
		argnames is ['Matrix', 'Norm']
	]).

	:- public(transpose_matrix/2).
	:- mode(transpose_matrix(+list(list(number)), -list(list(number))), one).
	:- info(transpose_matrix/2, [
		comment is 'Transposes a numeric matrix represented as a list of row lists.',
		argnames is ['Matrix', 'Transpose']
	]).

	:- public(vector_value/3).
	:- mode(vector_value(+list(number), +integer, -number), one).
	:- info(vector_value/3, [
		comment is 'Looks up a numeric vector element using a one-based index.',
		argnames is ['Vector', 'Index', 'Value']
	]).

	:- public(matrix_row/3).
	:- mode(matrix_row(+list(list(number)), +integer, -list(number)), one).
	:- info(matrix_row/3, [
		comment is 'Looks up a numeric matrix row using a one-based row index.',
		argnames is ['Matrix', 'RowIndex', 'Row']
	]).

	:- public(matrix_column/3).
	:- mode(matrix_column(+list(list(number)), +integer, -list(number)), one).
	:- info(matrix_column/3, [
		comment is 'Looks up a numeric matrix column using a one-based column index.',
		argnames is ['Matrix', 'ColumnIndex', 'Column']
	]).

	:- public(matrix_value/4).
	:- mode(matrix_value(+list(list(number)), +integer, +integer, -number), one).
	:- info(matrix_value/4, [
		comment is 'Looks up a numeric matrix element using one-based row and column indices.',
		argnames is ['Matrix', 'RowIndex', 'ColumnIndex', 'Value']
	]).

	:- public(matrix_diagonal/2).
	:- mode(matrix_diagonal(+list(list(number)), -list(number)), one).
	:- info(matrix_diagonal/2, [
		comment is 'Extracts the main diagonal of a numeric matrix as a vector.',
		argnames is ['Matrix', 'Diagonal']
	]).

	:- public(matrix_diagonal/3).
	:- mode(matrix_diagonal(+list(list(number)), +integer, -list(number)), one_or_error).
	:- info(matrix_diagonal/3, [
		comment is 'Extracts the diagonal of a numeric matrix at the given offset, where positive offsets select superdiagonals and negative offsets select subdiagonals.',
		argnames is ['Matrix', 'Offset', 'Diagonal']
	]).

	:- public(diagonal_matrix/2).
	:- mode(diagonal_matrix(+list(number), -list(list(number))), one).
	:- info(diagonal_matrix/2, [
		comment is 'Constructs a square diagonal matrix from a numeric vector of diagonal entries.',
		argnames is ['Diagonal', 'Matrix']
	]).

	:- public(diagonal_matrix/3).
	:- mode(diagonal_matrix(+list(number), +integer, -list(list(number))), one_or_error).
	:- info(diagonal_matrix/3, [
		comment is 'Constructs the minimal square numeric matrix whose diagonal at the given offset matches the input vector.',
		argnames is ['Diagonal', 'Offset', 'Matrix']
	]).

	:- public(diagonal_matrix/4).
	:- mode(diagonal_matrix(+list(number), +integer, +integer, -list(list(number))), one_or_error).
	:- info(diagonal_matrix/4, [
		comment is 'Constructs a square numeric matrix of the requested size whose diagonal at the given offset matches the input vector.',
		argnames is ['Diagonal', 'Offset', 'Size', 'Matrix']
	]).

	:- public(matrix_trace/2).
	:- mode(matrix_trace(+list(list(number)), -number), one).
	:- info(matrix_trace/2, [
		comment is 'Computes the trace of a numeric matrix by summing its main diagonal entries.',
		argnames is ['Matrix', 'Trace']
	]).

	:- public(shift_matrix_diagonal/3).
	:- mode(shift_matrix_diagonal(+list(list(number)), +number, -list(list(number))), one).
	:- info(shift_matrix_diagonal/3, [
		comment is 'Shifts the main diagonal of a numeric matrix by the given value.',
		argnames is ['Matrix', 'Shift', 'ShiftedMatrix']
	]).

	:- public(upper_triangular_part/2).
	:- mode(upper_triangular_part(+list(list(number)), -list(list(number))), one_or_error).
	:- info(upper_triangular_part/2, [
		comment is 'Returns the upper-triangular part of a numeric matrix including the main diagonal.',
		argnames is ['Matrix', 'UpperTriangular']
	]).

	:- public(upper_triangular_part/3).
	:- mode(upper_triangular_part(+list(list(number)), +integer, -list(list(number))), one_or_error).
	:- info(upper_triangular_part/3, [
		comment is 'Returns the upper-triangular part of a numeric matrix using the given diagonal offset, where positive offsets exclude leading diagonals and negative offsets include additional subdiagonals.',
		argnames is ['Matrix', 'Offset', 'UpperTriangular']
	]).

	:- public(lower_triangular_part/2).
	:- mode(lower_triangular_part(+list(list(number)), -list(list(number))), one_or_error).
	:- info(lower_triangular_part/2, [
		comment is 'Returns the lower-triangular part of a numeric matrix including the main diagonal.',
		argnames is ['Matrix', 'LowerTriangular']
	]).

	:- public(lower_triangular_part/3).
	:- mode(lower_triangular_part(+list(list(number)), +integer, -list(list(number))), one_or_error).
	:- info(lower_triangular_part/3, [
		comment is 'Returns the lower-triangular part of a numeric matrix using the given diagonal offset, where positive offsets include additional superdiagonals and negative offsets exclude leading diagonals below the main diagonal.',
		argnames is ['Matrix', 'Offset', 'LowerTriangular']
	]).

	:- public(solve_linear_system/3).
	:- mode(solve_linear_system(+list(list(number)), +list(number), -list(float)), one_or_error).
	:- info(solve_linear_system/3, [
		comment is 'Solves a non-singular square numeric linear system with a vector right-hand side using pivoted elimination.',
		argnames is ['Matrix', 'Values', 'Solution']
	]).

	:- public(solve_linear_systems/3).
	:- mode(solve_linear_systems(+list(list(number)), +list(list(number)), -list(list(float))), one_or_error).
	:- info(solve_linear_systems/3, [
		comment is 'Solves a non-singular square numeric linear system with a matrix right-hand side using pivoted elimination.',
		argnames is ['Matrix', 'RightHandSides', 'Solutions']
	]).

	:- public(determinant/2).
	:- mode(determinant(+list(list(number)), -number), one_or_error).
	:- info(determinant/2, [
		comment is 'Computes the determinant of a square numeric matrix.',
		argnames is ['Matrix', 'Determinant']
	]).

	:- public(inverse_matrix/2).
	:- mode(inverse_matrix(+list(list(number)), -list(list(float))), one_or_error).
	:- info(inverse_matrix/2, [
		comment is 'Computes the inverse of a non-singular square numeric matrix by solving one linear system per identity column.',
		argnames is ['Matrix', 'Inverse']
	]).

	:- public(qr_decomposition/3).
	:- mode(qr_decomposition(+list(list(number)), -list(list(float)), -list(list(float))), one_or_error).
	:- info(qr_decomposition/3, [
		comment is 'Computes a thin QR decomposition of a rectangular numeric matrix, returning orthogonal columns and an upper-trapezoidal factor.',
		argnames is ['Matrix', 'Orthogonal', 'UpperTriangular']
	]).

	:- public(least_squares/3).
	:- mode(least_squares(+list(list(number)), +list(number), -list(float)), one_or_error).
	:- info(least_squares/3, [
		comment is 'Computes a least-squares solution for a rectangular numeric linear system using a pivoted orthogonal solve without forming normal equations.',
		argnames is ['Matrix', 'Values', 'Solution']
	]).

	:- public(matrix_rank/2).
	:- mode(matrix_rank(+list(list(number)), -integer), one_or_error).
	:- info(matrix_rank/2, [
		comment is 'Estimates the rank of a numeric matrix using the default library numerical tolerance of 1.0e-12.',
		argnames is ['Matrix', 'Rank']
	]).

	:- public(matrix_rank/3).
	:- mode(matrix_rank(+list(list(number)), +number, -integer), one_or_error).
	:- info(matrix_rank/3, [
		comment is 'Estimates the rank of a numeric matrix using the given non-negative numerical tolerance.',
		argnames is ['Matrix', 'Tolerance', 'Rank']
	]).

	:- public(symmetric_eigen/3).
	:- mode(symmetric_eigen(+list(list(number)), -list(list(float)), -list(float)), one_or_error).
	:- info(symmetric_eigen/3, [
		comment is 'Computes an orthonormal eigendecomposition of a real symmetric dense matrix using the default library numerical tolerance of 1.0e-12.',
		argnames is ['Matrix', 'Eigenvectors', 'Eigenvalues']
	]).

	:- public(symmetric_eigen/4).
	:- mode(symmetric_eigen(+list(list(number)), +number, -list(list(float)), -list(float)), one_or_error).
	:- info(symmetric_eigen/4, [
		comment is 'Computes an orthonormal eigendecomposition of a real symmetric dense matrix using the given non-negative numerical tolerance.',
		argnames is ['Matrix', 'Tolerance', 'Eigenvectors', 'Eigenvalues']
	]).

	:- public(symmetric_eigen/5).
	:- mode(symmetric_eigen(+list(list(number)), +number, +integer, -list(list(float)), -list(float)), one_or_error).
	:- info(symmetric_eigen/5, [
		comment is 'Computes an orthonormal eigendecomposition of a real symmetric dense matrix using the given non-negative numerical tolerance and positive iteration bound.',
		argnames is ['Matrix', 'Tolerance', 'MaximumIterations', 'Eigenvectors', 'Eigenvalues']
	]).

	:- public(pseudo_inverse/2).
	:- mode(pseudo_inverse(+list(list(number)), -list(list(float))), one_or_error).
	:- info(pseudo_inverse/2, [
		comment is 'Computes the Moore-Penrose pseudo-inverse of a rectangular numeric matrix using the default library numerical tolerance of 1.0e-12.',
		argnames is ['Matrix', 'PseudoInverse']
	]).

	:- public(pseudo_inverse/3).
	:- mode(pseudo_inverse(+list(list(number)), +number, -list(list(float))), one_or_error).
	:- info(pseudo_inverse/3, [
		comment is 'Computes the Moore-Penrose pseudo-inverse of a rectangular numeric matrix using the given non-negative numerical tolerance.',
		argnames is ['Matrix', 'Tolerance', 'PseudoInverse']
	]).

	:- public(null_space/2).
	:- mode(null_space(+list(list(number)), -list(list(float))), one_or_error).
	:- info(null_space/2, [
		comment is 'Computes an orthonormal basis for the right null space of a rectangular numeric matrix using the default library numerical tolerance of 1.0e-12.',
		argnames is ['Matrix', 'Basis']
	]).

	:- public(null_space/3).
	:- mode(null_space(+list(list(number)), +number, -list(list(float))), one_or_error).
	:- info(null_space/3, [
		comment is 'Computes an orthonormal basis for the right null space of a rectangular numeric matrix using the given non-negative numerical tolerance.',
		argnames is ['Matrix', 'Tolerance', 'Basis']
	]).

	:- public(normalize_vector/2).
	:- mode(normalize_vector(+list(number), -list(number)), one).
	:- info(normalize_vector/2, [
		comment is 'Normalizes a numeric vector to unit length when its norm is above the default library numerical tolerance of 1.0e-12.',
		argnames is ['Vector', 'NormalizedVector']
	]).

	:- public(normalize_vector/3).
	:- mode(normalize_vector(+list(number), +number, -list(number)), one_or_error).
	:- info(normalize_vector/3, [
		comment is 'Normalizes a numeric vector to unit length when its norm is above the given non-negative numerical tolerance.',
		argnames is ['Vector', 'Tolerance', 'NormalizedVector']
	]).

	:- public(difference_norm/3).
	:- mode(difference_norm(+list(number), +list(number), -float), one).
	:- info(difference_norm/3, [
		comment is 'Computes the Euclidean norm of the difference between two numeric vectors.',
		argnames is ['Vector1', 'Vector2', 'Norm']
	]).

	:- public(stabilize_vector_sign/2).
	:- mode(stabilize_vector_sign(+list(number), -list(number)), one).
	:- info(stabilize_vector_sign/2, [
		comment is 'Normalizes a vector sign convention by flipping vectors whose first significant component is negative.',
		argnames is ['Vector', 'StableVector']
	]).

	:- public(stabilize_vector_sign/3).
	:- mode(stabilize_vector_sign(+list(number), +number, -list(number)), one_or_error).
	:- info(stabilize_vector_sign/3, [
		comment is 'Normalizes a vector sign convention by flipping vectors whose first significant component under the given non-negative numerical tolerance is negative.',
		argnames is ['Vector', 'Tolerance', 'StableVector']
	]).

	:- public(first_significant_component/2).
	:- mode(first_significant_component(+list(number), -number), one).
	:- info(first_significant_component/2, [
		comment is 'Returns the first component whose absolute value exceeds the default library numerical tolerance of 1.0e-12, defaulting to zero when no such component exists.',
		argnames is ['Vector', 'First']
	]).

	:- public(first_significant_component/3).
	:- mode(first_significant_component(+list(number), +number, -number), one_or_error).
	:- info(first_significant_component/3, [
		comment is 'Returns the first component whose absolute value exceeds the given non-negative numerical tolerance, defaulting to zero when no such component exists.',
		argnames is ['Vector', 'Tolerance', 'First']
	]).

	:- public(add_scaled_outer_product/4).
	:- mode(add_scaled_outer_product(+list(number), +number, +list(list(number)), -list(list(number))), one).
	:- info(add_scaled_outer_product/4, [
		comment is 'Adds a scaled outer product of a vector with itself to an accumulated matrix.',
		argnames is ['Vector', 'Scale', 'Matrix0', 'Matrix']
	]).

	:- public(covariance_matrix/2).
	:- mode(covariance_matrix(+list(list(number)), -list(list(number))), one_or_error).
	:- info(covariance_matrix/2, [
		comment is 'Computes the sample covariance matrix from a list of centered numeric row vectors.',
		argnames is ['Rows', 'CovarianceMatrix']
	]).

	:- public(cholesky_decomposition/2).
	:- mode(cholesky_decomposition(+list(list(number)), -list(list(float))), one_or_error).
	:- info(cholesky_decomposition/2, [
		comment is 'Computes the lower-triangular Cholesky factor of a positive definite numeric matrix.',
		argnames is ['Matrix', 'CholeskyFactor']
	]).

	:- public(solve_cholesky/3).
	:- mode(solve_cholesky(+list(list(number)), +list(number), -list(float)), one).
	:- info(solve_cholesky/3, [
		comment is 'Solves a positive definite linear system from a lower-triangular Cholesky factor and a right-hand-side vector.',
		argnames is ['CholeskyFactor', 'Values', 'Solution']
	]).

	:- public(forward_substitution/3).
	:- mode(forward_substitution(+list(list(number)), +list(number), -list(float)), one).
	:- info(forward_substitution/3, [
		comment is 'Solves a lower-triangular system by forward substitution.',
		argnames is ['LowerTriangular', 'Values', 'Solution']
	]).

	:- public(solve_lower_triangular_matrix/3).
	:- mode(solve_lower_triangular_matrix(+list(list(number)), +list(list(number)), -list(list(float))), one).
	:- info(solve_lower_triangular_matrix/3, [
		comment is 'Solves a lower-triangular system with a matrix right-hand side by forward substitution on each column.',
		argnames is ['LowerTriangular', 'Matrix', 'Solution']
	]).

	:- public(solve_upper_triangular/3).
	:- mode(solve_upper_triangular(+list(list(number)), +list(number), -list(float)), one).
	:- info(solve_upper_triangular/3, [
		comment is 'Solves an upper-triangular system by backward substitution.',
		argnames is ['UpperTriangular', 'Values', 'Solution']
	]).

	:- public(solve_upper_triangular_matrix/3).
	:- mode(solve_upper_triangular_matrix(+list(list(number)), +list(list(number)), -list(list(float))), one).
	:- info(solve_upper_triangular_matrix/3, [
		comment is 'Solves an upper-triangular system with a matrix right-hand side by backward substitution on each column.',
		argnames is ['UpperTriangular', 'Matrix', 'Solution']
	]).

	:- public(backward_substitution/3).
	:- mode(backward_substitution(+list(list(number)), +list(number), -list(float)), one).
	:- info(backward_substitution/3, [
		comment is 'Solves the transposed upper-triangular system induced by a lower-triangular Cholesky factor using backward substitution.',
		argnames is ['CholeskyFactor', 'Values', 'Solution']
	]).

	:- public(invert_from_cholesky/2).
	:- mode(invert_from_cholesky(+list(list(number)), -list(list(float))), one).
	:- info(invert_from_cholesky/2, [
		comment is 'Computes a matrix inverse from a Cholesky factor by solving one linear system per basis column.',
		argnames is ['CholeskyFactor', 'Inverse']
	]).

:- end_protocol.
