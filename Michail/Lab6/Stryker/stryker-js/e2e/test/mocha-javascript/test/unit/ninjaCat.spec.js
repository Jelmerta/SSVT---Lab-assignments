const ninjaCatSays = require('../../src/π±βπninja.cat');
const { expect } = require('chai');

describe('π±βπ', () => {
  describe('ninjaCatSays', () => {
    it('should speak', () => {
      expect(ninjaCatSays('ATTACK!')).eq('π±βπ ATTACK!')
    });
  });
});
