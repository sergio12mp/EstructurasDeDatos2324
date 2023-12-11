package dataStructures.tree;

import dataStructures.list.List;

public class Tree<T> {
    private static class Node<E> {
        E elem;
        List<Node<E>> children;

        public Node(E elem, List<Node<E>> children) {
            this.elem = elem;
            this.children = children;
        }
    }

    Node<T> root;

    public Tree() {
        root = null;
    }

    public int size() {
        return size(root);
    }
    //Funcion estatica generica
    private static <T> int size(Node<T> node) {
        if (node == null)
            return 0;
        else {
            int sum = 1;
            for (Node<T> t : node.children)
                sum += size(t);
            return sum;
        }
    }

    public int height() {
        return height(root);
    }
    //Funcion estatica generica
    private static <T> int height(Node<T> node) {
        if (node == null)
            return 0;
        else {
            int max = 0;//El profesor lo pone en -1 pero no se porque
            
            for (Node<T> t : node.children)
                max = Math.max(max, height(t));
            return max + 1;
        }
    }


}
