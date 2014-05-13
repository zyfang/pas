package actSimG_prologMongo;

import org.jgrapht.graph.DefaultEdge;

public class RelationEdge<T> extends DefaultEdge {
	
	private T node1;
	private T node2;
	private String label;
	
	public RelationEdge(T node1, T node2, String label) {
        this.node1 = node1;
        this.node2 = node2;
        this.label = label;
    }

    public T getNode1() {
        return node1;
    }

    public T getNode2() {
        return node2;
    }

    public String toString() {
        return label;
    }

}
