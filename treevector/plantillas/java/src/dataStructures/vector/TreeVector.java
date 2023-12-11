/******************************************************************************
 * Student's name:
 * Student's group:
 * Data Structures. Grado en Informática. UMA.
 ******************************************************************************/

package dataStructures.vector;

import dataStructures.list.ArrayList;
import dataStructures.list.List;

public class TreeVector<T> {

    private final int size;
    private final Tree<T> root;

    private interface Tree<E> {
        E get(int index);

        void set(int index, E x);

        List<E> toList();
    }

    private static class Leaf<E> implements Tree<E> {
        private E value;

        private Leaf(E x) {
            value = x;
        }

        @Override
        public E get(int index) {
            return value;
        }

        @Override
        public void set(int i, E x) {
            value = x;
        }

        @Override
        public List<E> toList() {
            List<E> res = new ArrayList<>();
            res.append(value);
            return res;
        }
    }

    private static class Node<E> implements Tree<E> {
        private Tree<E> left;
        private Tree<E> right;

        private Node(Tree<E> l, Tree<E> r) {
            left = l;
            right = r;
        }

        @Override
        public E get(int index) {
            if (index % 2 == 0) {
                return left.get(index / 2);
            } else {
                return right.get(index / 2);
            }
        }

        @Override
        public void set(int index, E x) {
            if (index % 2 == 0) {
                left.set(index / 2, x);
            } else {
                right.set(index / 2, x);
            }
        }

        @Override
        public List<E> toList() {
            return intercalate(left.toList(), right.toList());
        }
    }

    public TreeVector(int n, T value) {
        if (n < 0)
            throw new VectorException("Negative size");

        size = (int) Math.pow(2, n);

        if (size == 1) {
            root = new Leaf<>(value);
        } else {//Restamos -1 pq el arbol tiene 1 menos de altura que antes
            root = new Node<>(new TreeVector<>(n - 1, value).root, new TreeVector<>(n - 1, value).root);
        }
    }

    private int calcularPotencia(int n) {
        if (n == 0)
            return 1;
        return 2 * calcularPotencia(n - 1);
    }

    public int size() {
        if (root == null)
            return 0;
        return size;
    }

    public T get(int i) {
        if (root == null || i < 0 || i >= size)
            throw new VectorException("Arbol nulo o indice invalido");

        return root.get(i);
    }

    public void set(int i, T x) {
        if (root == null || i < 0 || i >= size)
            throw new VectorException("Arbol nulo o indice invalido");

        root.set(i, x);
    }

    public List<T> toList() {
        return root.toList();
    }

    protected static <E> List<E> intercalate(List<E> xs, List<E> ys) {
        List<E> res = new ArrayList<>();
        int i = 0;
        if (xs == null || ys == null)
            throw new VectorException("Lista nula");
        while (i < xs.size() && i < ys.size()) {
            res.append(xs.get(i));
            res.append(ys.get(i));
            i++;

        }
        return res;
    }


    // Only for students not taking continuous assessment

    static protected boolean isPowerOfTwo(int n) {
        //to do
        return false;
    }

    public static <E> TreeVector<E> fromList(List<E> l) {
        //to do
        return null;
    }
}
