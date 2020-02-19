package org.ergoplatform.local

import org.ergoplatform.modifiers.history.popow.{PoPowProof, PoPowHeader, PoPowParams}
import scorex.util.ModifierId
import org.ergoplatform.modifiers.history.popow.PoPowParams

class PoPoWVerifier(poPoWParams: PoPowParams, genesisId: ModifierId) {
    // TODO: ideally prefix would have the actual genesis but may just not have it
    var bestProof = PoPowProof(poPoWParams.m, poPoWParams.k, Seq(), Seq())

    def bestChain(): Seq[PoPowHeader] = {
        bestProof.chain
    }

    def process(newProof: PoPowProof) {
        if (newProof.chain.head.id == genesisId) {
            // TODO: order matters here and it shouldn't
            // only the isBetterThan's `this` is checked for validity
            if (!bestProof.isBetterThan(newProof) ||
                (bestProof.chain.length == 0 && newProof.isValid())) {
                bestProof = newProof
            }
        }
    }
}