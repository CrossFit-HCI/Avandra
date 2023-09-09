import React, { ReactNode } from 'react';
import { Text, View} from 'react-native';
import { NativeStackScreenProps } from '@react-navigation/native-stack';

import Nav, { NaviButton } from "../Nav/view";
import { RootViews } from '../RootViews';
import { navCustomViewStyleSheet } from '../Nav/styles';

type JournalsScreenProps= NativeStackScreenProps<RootViews, 'Journals'>;

const Journals = ({ navigation }:JournalsScreenProps) => {
  let navMainScreen = () => {
    return (
      <View style={navCustomViewStyleSheet.container}>
        <NaviButton
          onPress={() => navigation.navigate('Forecast')}
          title="Forecast" />
      </View>
    )
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
  )
};

export default Journals;