import React from 'react';
import { Text, View} from 'react-native';
import { NativeStackScreenProps } from '@react-navigation/native-stack';

import Nav, { NavScreen, NaviButton } from "../nav/view";
import { CloseScreen, OpenScreen } from '../nav/model';

import { RootViews } from '../RootViews';
import { forecastStyle } from './styles';

type ForecastScreenProps = NativeStackScreenProps<RootViews, 'Forecast'>;

const ModalScreen = () => {
  return (
    <View>
      <Text style={ forecastStyle.heading1 }>I'm a modal.</Text>

          <CloseScreen title='Close Modal1'/>
    </View>
  )
}

const Forecast = ({ navigation }:ForecastScreenProps) => {
  let navMainScreen = () => {
    return (
      <View>
        <NaviButton
          onPress={() => navigation.navigate('Journals')}
          title="Journals" />         
      </View>
    )
  }

  return (
      <View style={ forecastStyle.component }> 
        <Text style={ forecastStyle.heading1 }>Forecast</Text>

        <OpenScreen
          label='Modal1'
          title="Open Modal1" /> 

        <Nav main={navMainScreen()}>
            <NavScreen label='Modal1'  screen={ModalScreen()} />
        </Nav>                
      </View>
  )
}

export default Forecast;