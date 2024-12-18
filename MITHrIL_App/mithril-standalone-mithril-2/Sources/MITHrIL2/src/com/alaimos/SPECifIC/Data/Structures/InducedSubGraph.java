package com.alaimos.SPECifIC.Data.Structures;

import com.alaimos.MITHrIL.Data.Pathway.Interface.EdgeInterface;
import com.alaimos.MITHrIL.Data.Pathway.Interface.GraphInterface;
import com.alaimos.MITHrIL.Data.Pathway.Interface.NodeInterface;
import com.alaimos.MITHrIL.Data.Pathway.Interface.PathwayInterface;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * A non-navigable object which represents the induced sub-graph generated by a VisitTree
 *
 * @author Salvatore Alaimo, Ph.D.
 * @version 2.0.0.0
 * @since 16/12/2016
 */
public class InducedSubGraph {

    protected VisitTree        tree;
    protected double           accumulator;
    protected NodeInterface[]  nodes;
    protected EdgeInterface[]  edges;
    protected double[]         perturbations;
    protected double[]         pValues;
    protected double           pValue;
    protected PathwayInterface origin;
    protected double           adjustedPValue;

    public InducedSubGraph(VisitTree tree, PathwayInterface origin) {
        this.tree = tree;
        this.origin = origin;
        this.init();
    }

    InducedSubGraph() {
    }

    protected void init() {
        nodes = tree.stream().map(n -> n.getObject().getNode()).toArray(NodeInterface[]::new);
        perturbations = tree.stream().mapToDouble(n -> n.getObject().getPerturbation()).toArray();
        pValues = tree.stream().mapToDouble(n -> n.getObject().getPValue()).toArray();
        computeAccumulator();
        computeEdges();
        pValue = tree.getTreePValue();
    }

    void computeAccumulator() {
        accumulator = 0.0;
        for (int i = 0; i < nodes.length; i++) {
            accumulator += nodes[i].getType().sign() * perturbations[i];
        }
    }

    void computeEdges() {
        GraphInterface g = origin.getGraph();
        HashSet<EdgeInterface> edges = new HashSet<>();
        Map<String, NodeInterface> nodes =
                Arrays.stream(this.nodes).collect(Collectors.toMap(NodeInterface::getId, t -> t));
        for (NodeInterface n : nodes.values()) {
            g.outgoingNodesStream(n).forEach(v -> {
                if (nodes.containsKey(v.getId())) {
                    edges.add(g.getEdge(n, v));
                }
            });
        }
        this.edges = edges.toArray(new EdgeInterface[edges.size()]);
    }

    public double getAccumulator() {
        return accumulator;
    }

    public NodeInterface[] getNodes() {
        return nodes;
    }

    public EdgeInterface[] getEdges() {
        return edges;
    }

    public double[] getPerturbations() {
        return perturbations;
    }

    public double[] getPValues() {
        return pValues;
    }

    public double getGraphPValue() {
        return pValue;
    }

    public double getAdjustedPValue() {
        return adjustedPValue;
    }

    public void setAdjustedPValue(double adjustedPValue) {
        this.adjustedPValue = adjustedPValue;
    }
}
