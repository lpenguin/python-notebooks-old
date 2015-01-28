#!/usr/bin/env python

def LEADERBOARDCYCLOPEPTIDESEQUENCING(Spectrum, N):
    Leaderboard = [[]]
    LeaderPeptide = []
    i = 1
    while Leaderboard:
        print "iteration %d" % i
        Leaderboard = Expand(Leaderboard)
        for Peptide in Leaderboard[:]:
            if Mass(Peptide) == ParentMass(Spectrum):
                # if Score(Peptide, Spectrum) > Score(LeaderPeptide, Spectrum):
                LeaderPeptide.append(dict(peptide=Peptide, score=Score(Peptide, Spectrum)))
            elif Mass(Peptide) > ParentMass(Spectrum):
                Leaderboard.remove(Peptide)
        Leaderboard = Cut(Leaderboard, Spectrum, N)
        i += 1
    return sorted(LeaderPeptide, key= lambda x: -x['score'])

import collections

def Expand(Leaderboard):
    masses = (57,71,87,97,99,101,103,113,114,115,128,129,131,137,147,156,163,186)
    return [ [m]+x for m in masses for x in Leaderboard]

def Mass(Peptide):
    return sum(Peptide)

def ParentMass(Spectrum):
    return max(Spectrum)

def linearSpectrum(peptide):
    peptideLen = len(peptide)
    for l in xrange(1, peptideLen + 1):
        for i in xrange(0, peptideLen - l):
            yield sum(peptide[i:i+l])
from collections import Counter
def LinearScore(Peptide, Spectrum):
    cs = linearSpectrum(Peptide)
    # common = set(Spectrum).intersection(cs)
    # csc = collections.Counter(cs)
    # Spectrumc = collections.Counter(Spectrum)
    # return sum(min(csc[v],Spectrumc[v]) for v in common)
    t = Counter(cs)
    e = Counter(Spectrum)
    c = t - e
    return sum(t.values()) - sum(c.values())

def Score(Peptide, Spectrum):
    def cyclo_spectrum(peptide):
        if not peptide:
            return [0]
        cyclic_masses = peptide + peptide
        n = len(peptide)
        spectrum = [0,sum(peptide)]
        for i in xrange(1,n):
            for j in xrange(n):
                spectrum.append(sum(cyclic_masses[j:j+i]))
        return spectrum
    cs = cyclo_spectrum(Peptide)
    common = set(Spectrum).intersection(cs)
    csc = collections.Counter(cs)
    Spectrumc = collections.Counter(Spectrum)
    return sum(min(csc[v],Spectrumc[v]) for v in common)

def Cut(Leaderboard, Spectrum, N):
    if len(Leaderboard) <= N:
        return Leaderboard
    A = [(LinearScore(x,Spectrum),x) for x in Leaderboard]
    A.sort(reverse=True)
    return [x for s,x in A if s>=A[N-1][0]]

import argparse

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--input")
    parser.add_argument("--output")
    args = parser.parse_args()
    input_file = args.input
    output_file = args.output
    with open(input_file,'r') as f:
        N = int(f.readline().strip())
        spectrum = [int(w) for w in f.readline().strip().split()]
    result = LEADERBOARDCYCLOPEPTIDESEQUENCING(spectrum, N)

    for res in result:
        print res
    # with open(output_file,'w') as f:
    #     print >>f , '-'.join(str(w) for w in result)

if __name__ == '__main__':
    main()
