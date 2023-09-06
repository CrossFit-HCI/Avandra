import React from 'react';
import { Text, View} from 'react-native';
import { NativeStackScreenProps } from '@react-navigation/native-stack';


import Nav, { NavScreen, NaviButton } from "../nav/view";
import { CloseNavModal } from '../nav/model';
import { navCustomViewStyleSheet } from '../nav/styles';

import { RootViews } from '../RootViews';
import { forecastStyle } from './styles';
import { Card, CardForecastHeader } from '../components/card';

type ForecastScreenProps = NativeStackScreenProps<RootViews, 'Forecast'>;

const ModalScreen = () => {
  return (
    <View>
      <Text style={ forecastStyle.heading1 }>I'm a modal.</Text>

      <CloseNavModal title='Close Modal1'/>
    </View>
  )
}

const Forecast = ({ navigation }:ForecastScreenProps) => {
  let navMainScreen = () => {
    return (
      <View style={navCustomViewStyleSheet.container}>
        <NaviButton
          onPress={() => navigation.navigate('Journals')}
          title="Journals" />         
      </View>
    )
  }

  /* How to open a modal: 
   * <OpenNavModal label='Modal1' title="Open Modal1" />  */
  return (
      <View style={ forecastStyle.component }> 
        <Card header={CardForecastHeader({markCompleted: false})}>
        </Card>

        <Nav main={navMainScreen()}>
            <NavScreen label='Modal1'  screen={ModalScreen()} />
        </Nav>                
      </View>
  );
};

export default Forecast;