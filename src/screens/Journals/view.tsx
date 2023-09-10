import React from 'react';
import { Text, View} from 'react-native';
import { NativeStackScreenProps } from '@react-navigation/native-stack';

import Nav, { ControlButtonNav } from '../Nav/view';
import { RootViews } from '../../RootViews';
import { navCustomViewStyleSheet } from '../Nav/styles';

type JournalsScreenProps = NativeStackScreenProps<RootViews, 'Journals'>;

const Journals = ({ navigation }: JournalsScreenProps) => {
  const navMainScreen = () => {
    return (
      <View style={navCustomViewStyleSheet.container}>
        <ControlButtonNav
          onPress={() => navigation.navigate('Forecast')}
          title="Forecast" />
      </View>
    );
  };

  return (    
      <View
        style={{
          flex: 1,
          justifyContent: 'center',
          alignItems: 'center',
      }}>
        <Text>Journal List</Text>

        <Nav main={navMainScreen()}>          
        </Nav>
      </View>
  );
};

export default Journals;