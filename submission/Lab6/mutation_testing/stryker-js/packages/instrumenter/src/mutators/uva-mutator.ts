import * as types from '@babel/types';

import { NodeMutator } from '.';

enum UpdateOperators {
  '+=' = '-=',
  '-=' = '+=',
}

export const uvaMutator: NodeMutator = {
  name: 'UvaMutator',

  *mutate(path) {
    if (path.isAssignmentExpression() && isSupported(path.node.operator)) {
      const mutatedOperator = UpdateOperators[path.node.operator];

      const replacement = types.cloneNode(path.node, true);
      replacement.operator = mutatedOperator;
      yield replacement;
    }
  },
};

function isSupported(operator: string): operator is UpdateOperators {
  return Object.keys(UpdateOperators).includes(operator);
}
