#include <stdlib.h>
#include <stdio.h>

struct Matrix_Int {
  int __numDimensions;
  int* __dimSize;
  int* __data;
};

struct Matrix_UInt {
  int __numDimensions;
  int* __dimSize;
  unsigned int* __data;
};

/*
 * Non-destructive addition of a matrix and integer
 */
struct Matrix_Int matrixScalarAdd(struct Matrix_Int m, int s) {

  // Construct the output matrix
  struct Matrix_Int out;
  out.__numDimensions = m.__numDimensions;
  out.__dimSize = (int*) malloc( sizeof(int) * m.__numDimensions );

  // Return error if malloc failed
  if (out.__dimSize == NULL) {
    printf("\n%s\n", "Error: Can't malloc for dimSize");
    exit(99);
  }

  // Find the number of elements in m while setting the size
  // of each dimension in the output matrix
  int i;
  int numOfElements = 1;
  for (i = 0; i < m.__numDimensions; i++) {
    numOfElements *= m.__dimSize[i];
    out.__dimSize[i] = m.__dimSize[i];
  }

  // Malloc space for the output matrix's data
  out.__data = (int*) malloc( sizeof(int) * numOfElements );

  // Return error if malloc failed
  if (out.__data == NULL) {
    printf("\n%s\n", "Error: Can't malloc for __data");
    exit(98);
  }

  // Iterate over every element in m and add s to it
  for (i = 0; i < numOfElements; i++) {
    out.__data[i] = m.__data[i] + s;
  }

  return out;
}

/*
 * Non-destructive matrix component-wise addition
 */
struct Matrix_Int matrixMatrixAdd(struct Matrix_Int m, struct Matrix_Int n) {

  struct Matrix_Int out;

  // Confirm they are each the same size and shape  
  if (m.__numDimensions != n.__numDimensions) {

    // Not matching dimensions, so return an error
    printf("\n%s\n", "Error: Non-matching dimensions in component-wise matrix addition");
    exit(1);

  } else {

    // Both matrices have the same number of dimensions
    // Assign numDimensions for output and malloc dimSize
    out.__numDimensions = m.__numDimensions;
    out.__dimSize = (int*) malloc( sizeof(int) * out.__numDimensions );

    // Return error if malloc failed
    if (out.__dimSize == NULL) {
      printf("\n%s\n", "Error: Can't malloc for __dimSize");
      exit(99);
    }

    // Confirm both matrices have the same size dimensions while
    // totaling number of elements
    int i;
    int numOfElements = 1;
    for (i = 0; i < m.__numDimensions; i++) {

      if (m.__dimSize[i] != n.__dimSize[i]) {

	// Not matching dimension, so return an error
	printf("\n%d\n", "Error: Non-matching dimension in component-wise matrix addition");
	exit(2);

      } else {

	// Matching dimension
	// Assign outputs' dimSize and increase numOfElements
	out.__dimSize[i] = m.__dimSize[i];
	numOfElements *= out.__dimSize[i];

      }
    }

    // Malloc output's data
    out.__data = (int*) malloc( sizeof(int) * numOfElements );

    // Return error if malloc failed
    if (out.__data == NULL) {
      printf("\n%s\n", "Error: Can't malloc for __data");
      exit(98);
    }

    // Assign every element of output's data
    for (i = 0; i < numOfElements; i++) {
      out.__data[i] = m.__data[i] + n.__data[i];
    }

    return out;
  }
}
