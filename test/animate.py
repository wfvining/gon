"""Animate the overlay network."""

from collections import defaultdict
import csv

from matplotlib import pyplot as plt, animation
import networkx as nx


def _load_positions(path):
    with open(path, 'r') as f:
        reader = csv.reader(f)
        _ = next(reader)
        return {n.strip("'"): (float(x), float(y)) for (n, x, y) in reader}


def _load_graph(path):
    edges = {}
    with open(path, 'r') as f:
        for line in f:
            l = line.strip().split(" ")
            ts = l[0]
            src = l[1]
            if len(l) == 2:
                dest = ""
            else:
                dest = l[2]
            t = int(ts)
            if t not in edges:
                edges[t] = []
            edges[t].extend((src, d) for d in dest.split(",") if d != "")
    return edges


def _draw(pos, edgedict):
    fig = plt.figure(figsize=(8,8))
    G = nx.DiGraph()
    G.add_nodes_from(pos.keys())

    nx.draw(G, pos=pos, with_labels=False, node_size=10)

    def animate(frame):
        fig.clear()
        G.remove_edges_from(edgedict[frame * 500])
        G.add_edges_from(edgedict[(frame + 1) * 500])
        nx.draw(G, pos=pos, with_labels=False, node_size=10)

    anim = animation.FuncAnimation(
        fig, animate, frames=round(600000 / 500) - 1, interval=20, repeat=True)

    writer = animation.FFMpegFileWriter(fps=60)
    anim.save("animation.mp4", writer=writer)


if __name__ == '__main__':
    pos = _load_positions("pos.csv")
    # pos[''] = (0.0, 0.0)
    edges = _load_graph("adjacency.dat")
    _draw(pos, edges)
