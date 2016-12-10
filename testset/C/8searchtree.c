/* Árbol de busqueda de caracteres.
 *
 * El programa debe operar en un ciclo infinito, preguntando al usuario si
 * desea continuar o terminar. Si desea continuar, el programa preguntara la
 * siguiente operacion, la efectuara, y reresara a la pregunta inicial. Las
 * operaciones posibles son:
 *
 * * Insertar un valor en el arbol.
 * * Eliminar un valor del arbol.
 * * Buscar un valor en el arbol.
 * * Mostrar el arbol.
 *
 * Para las operaciones, Ud. debe diseñar un tipo de datos usando registros,
 * tales que contenga:
 *
 * * El valor almacenado -- un caracter.
 * * La cantidad de repeticiones -- un entero. Si el usuario inserta varias
 *   veces el mismo caracter, se incrementa este contador. Si el usuario
 *   elimina el caracter, se decrementa el contador hasta llegar a cero; y
 *   en ese caso hay que sacar el nodo del arbol.
 * * Hijo izquierdo e hijo derecho, como apuntadores.
 *
 * El arbol debe ser mostrado como
 *
 *    (v0,n0)
 *      (v1,n1)
 *         (v2,n2)
 *         -
 *      (v3,n3)
 *         (v4,n4)
 *         (v5,n5)
 *
 * donde vI indica el valor (caracter) almacenado en el nodo, y nI el contador
 * de repeticiones. Los nodos se muestran en preorden, primero mostrando el
 * hijo izquierdo, y luego el derecho. Si un nodo no tiene hijos, no se
 * muestra nada especial; si le falta algun hijo, se muestra - en su lugar.
 *
 * Este programa debe ejercitar malloc para la insercion de nuevos nodos,
 * y free para la eliminacion de nodos tan pronto su conteo llega a cero.
 *
 * Como se trata de un arbol de busqueda, el valor almacenado en un nodo
 * debe ser estrictamente mayor que los valores almacenados en su subarbol
 * izquierdo, y estrictamente menor que los valores almacenados en su
 * subarbol derecho.
 */
