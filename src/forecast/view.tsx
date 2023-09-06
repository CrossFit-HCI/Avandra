import React from 'react';
import { Pressable, Text, View} from 'react-native';
import { NativeStackScreenProps } from '@react-navigation/native-stack';


import Nav, { NavScreen, NaviButton } from "../nav/view";
import { CloseNavModal } from '../nav/model';
import { navCustomViewStyleSheet } from '../nav/styles';

import { RootViews } from '../RootViews';
import { forecastStyle } from './styles';
import { Card } from '../components/card';
import Icon from 'react-native-vector-icons/MaterialCommunityIcons';
import { cardHeaderButtonTheme } from '../theme/Button';

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

  let CardForecastHeader = (markCompleted: boolean) => {
      let uncompletedIcon = 'checkbox-blank-circle-outline';
      let completedIcon = 'checkbox-marked-circle';
      let icon = markCompleted ? completedIcon : uncompletedIcon;

      return (
          <>
              <View style={ forecastStyle.component.card.header.completedContainer }>
                  <Icon name={ icon } size={30} />
              </View>
              <View style={ forecastStyle.component.card.header.headerTextForecastContainer }>
                  <Text style={ forecastStyle.component.card.header.headerText }>Journal Title</Text>
              </View>
              <Pressable style={ cardHeaderButtonTheme.container }>
                  <Text style={ cardHeaderButtonTheme.text }>Go!</Text>
              </Pressable>
          </>
      );
  };

  /* How to open a modal: 
   * <OpenNavModal label='Modal1' title="Open Modal1" />  */
  return (
      <View style={ forecastStyle.component }> 
        <Text style={{fontSize: 35}}>Today (09/06)</Text>

        <Card header={CardForecastHeader(false)}>
          <View style={{flex: 1, gap: 10, padding: 10}}>
            {[1,2,3,4,5,6,7,8,9,10,11,12].map((i) => (
              <>
              <Text style={{fontSize: 16}}>{i < 10 ? ` ${i}` : i}. Training Block</Text>
              <View style={{backgroundColor: 'rgba(0, 0, 0, .2)', width: '100%', height: i < 12 ? 1 : 0}}/>
              </>
            ))}
          </View>
        </Card>

        <Nav main={navMainScreen()}>
            <NavScreen label='Modal1'  screen={ModalScreen()} />
        </Nav>                
      </View>
  );
};

export default Forecast;