package com.typelead.gradle.utils;

import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

public class ImmutableDAG<K,V> {

    public Map<K,NodeInfo<K,V>> graph;

    private ImmutableDAG(Map<K,NodeInfo<K,V>> graph) {
        this.graph = graph;
    }

    public static <K,V> ImmutableDAG<K,V>
        create(Map<K,V> keyValues, Map<K,? extends Iterable<K>> dependencies) {
        Map<K,NodeInfo<K,V>> graph = new LinkedHashMap<K,NodeInfo<K,V>>();

        /* First phase, build out the skeleton of the DAG. */
        for (Map.Entry<K,V> entry: keyValues.entrySet()) {
            K key = entry.getKey();
            int i = 0;
            Iterable<K> deps = dependencies.get(key);
            for (Iterator<K> it = deps.iterator(); it.hasNext(); it.next(), i++) { }
            NodeInfo<K,V> nodeInfo = new NodeInfo(key, entry.getValue(),
                                                  new ArrayList<NodeInfo<K,V>>(i));
            graph.put(key, nodeInfo);
        }

        /* Second phase, populate the node infos. */
        for (Map.Entry<K,V> entry: keyValues.entrySet()) {
            K key = entry.getKey();
            NodeInfo<K,V> nodeInfo = graph.get(key);
            Iterable<K> deps = dependencies.get(key);
            Iterator<K> it = deps.iterator();
            List<NodeInfo<K,V>> resolvedDeps = new LinkedList<NodeInfo<K,V>>();
            while (it.hasNext()) {
                K depKey = it.next();
                NodeInfo<K,V> depNodeInfo = graph.get(depKey);
                if (depNodeInfo == null) {
                    throw new IllegalArgumentException
                        ("Unable to find node info for dependent node with key '"
                         + depKey.toString() + "'");
                }
                resolvedDeps.add(depNodeInfo);
            }
            nodeInfo.setDependentNodes(resolvedDeps);
        }

        return new ImmutableDAG(graph);
    }

    /* Returns the dependency closure of values corresponding to the given
       keys. */
    public List<V> closure(Collection<K> keys) {
        return differenceClosure(keys, Collections.emptySet());
    }

    /* Returns the dependency closure of values corresponding to the given
       keys, ignoring the part of the closure marked by excludedKeys. */
    public List<V> differenceClosure(Collection<K> keys, Set<K> excludedKeys) {
        /* Stores the output of this function. */
        List<V> results              = new ArrayList<V>();

        /* Stores the list of nodes that we have already traversed. */
        Set<K> traversed = new HashSet<K>(excludedKeys);

        /* Stores the list of nodes we have yet to traverse */
        Queue<NodeInfo<K,V>> queue   = new ArrayDeque<NodeInfo<K,V>>(2 * keys.size());

        /* Initializes the work queue. */
        for (K key : keys) {
            if (!excludedKeys.contains(key)) {
                traversed.add(key);
                queue.offer(graph.get(key));
            }
        }

        while (!queue.isEmpty()) {
            NodeInfo<K,V> current = queue.poll();
            results.add(current.getValue());
            for (NodeInfo<K,V> nodeInfo : current.getDependentNodes()) {
                final K nodeKey = nodeInfo.getKey();
                if (!traversed.contains(nodeKey)) {
                    traversed.add(nodeKey);
                    queue.offer(nodeInfo);
                }
            }
        }
        return results;
    }

    public Collection<V> getAllValues() {
        List<V> results = new ArrayList<V>();
        for (NodeInfo<K,V> nodeInfo: graph.values()) {
            results.add(nodeInfo.getValue());
        }
        return results;
    }

    private static class NodeInfo<K,V> {
        private final K key;
        private final V value;
        private final ArrayList<NodeInfo<K,V>> dependentNodes;

        public NodeInfo(final K key, final V value,
                        final ArrayList<NodeInfo<K,V>> dependentNodes) {
            this.key = key;
            this.value = value;
            this.dependentNodes = dependentNodes;
        }

        public K getKey() {
            return key;
        }

        public V getValue() {
            return value;
        }

        public Iterable<NodeInfo<K,V>> getDependentNodes() {
            return dependentNodes;
        }

        public void setDependentNodes(Collection<NodeInfo<K,V>> nodes) {
            dependentNodes.addAll(nodes);
        }

        @Override
        public boolean equals(Object o) {
            if (o instanceof NodeInfo) {
                return ((NodeInfo) o).getKey().equals(getKey());
            }
            return false;
        }
    }

}
