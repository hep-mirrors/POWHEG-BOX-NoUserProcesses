#include "splitandfilt.hh"
#include "fastjet/tools/Filter.hh"

using namespace std;
using namespace fastjet;

// Short description of the Split and filter routine:
string splitandfilt::description() const{
  ostringstream oss;

  oss << "This is the Split and Filter routine with mu_cut = " << _mu_cut
      << " and v_cut = " << _v_cut;

  return oss.str();

}

PseudoJet splitandfilt::result(const PseudoJet &jet) const{

  PseudoJet j1,j2;
  PseudoJet filtered_jet;

  _recurse_through_jet(jet,j1,j2,jet);

// If there is no parent, the jet does not qualify for one which does
// have substructure:
  if (j1 == 0) return PseudoJet();

// We recluster the constituents of the jet using the CA-algorithm:
// We extract the constituents:
  vector<PseudoJet> particles = jet.constituents();

// We set up the jet algorithm:
  double Rfilt = min(_dRj1j2,sqrt(j1.squared_distance(j2))/2.0);
  JetDefinition jet_def(cambridge_algorithm,Rfilt);

// We perform the clustering:
  ClusterSequence cs(particles,jet_def);
// We sort the subjets according to their pt:
  vector<PseudoJet> subjets = sorted_by_pt(cs.inclusive_jets());
  int nsubjets = subjets.size();
  for (int i = 0; i < min(nsubjets,3); i++){
    if (i == 0) filtered_jet = subjets[i];
// Caveat: Check the userinfo propagation into filtered_jet:
    if (i != 0) filtered_jet = join(filtered_jet,subjets[i]);
  }

// Give back the new filtered jet:
  return filtered_jet;
    
}

inline void splitandfilt::_recurse_through_jet(const PseudoJet &jet, 
                                               PseudoJet &j1, PseudoJet &j2, 
					       const PseudoJet &original_jet) const{

  PseudoJet parent1 = PseudoJet();
  PseudoJet parent2 = PseudoJet();

// We undo the last clustering step to obtain the parents of jet,
// if there is no one simply return:
  if (! jet.has_parents(parent1, parent2)) return;
// We need one extra condition, there should be a minimal separation 
// between the parents:
  double dRj1j2 = sqrt(parent1.squared_distance(parent2));
  if (dRj1j2 < _dRj1j2) return;

// The parents should be ordered in mass:
  if (parent1.m() < parent2.m()) swap(parent1,parent2);

// We calculate mu and v:
  double mu = parent1.m() / jet.m();
  double v  = min(parent1.perp2(),parent2.perp2())/jet.m2() *
              parent1.squared_distance(parent2);

// if mu > _mu_cut and/or v is < _v_cut we have to run the declustering
// again, but with j1:
  if (mu > _mu_cut || v < _v_cut) _recurse_through_jet(parent1,j1,j2,jet);
// Otherwise the two subjets are parent1 and parent2, put them into j1, j2:
  else {
    j1 = parent1;
    j2 = parent2;
    return;
  }
}
