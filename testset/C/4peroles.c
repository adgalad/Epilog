/* Orientacion a Peroles.
 * Considere un tipo de datos
 *
 *   struct {
 *     int  tipo;
 *     union {
 *       bool  b;
 *       char  c;
 *       int   i;
 *       float f;
 *     }
 *   }
 *
 * El programa debe operar en un ciclo infinito, preguntando al usuario si
 * quiere organizar elementos, o terminar. Si el usuario indica que quiere
 * organizar elementos, debe preguntar cuantos elementos organizar, y luego
 * solicitarlos uno por uno. Al solicitar cada elemento debe preguntarse el
 * tipo particular antes de leerlo.
 * Los elementos leidos deben almacenarse en un arreglo, y luego deben
 * reorganizarse (usando la tecnica de la «Bandera Holandesa») para tener
 * primero los booleanos, luego los caracteres, siguiendo los enteros y
 * finalmente los flotantes, antes de imprimir el arreglo en pantalla
 * (un elemento por linea).
 * Su programa debe ser capaz de operar con no mas de veinte (20) elementos
 * en el arreglo y todo el trabajo debe completarse sobre el mismo arreglo.
 */
