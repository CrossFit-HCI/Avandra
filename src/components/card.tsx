import { GestureResponderEvent, Pressable, Text, View, ViewStyle } from "react-native";
import { ReactNode } from "react";

import Icon from "react-native-vector-icons/MaterialCommunityIcons";

import { cardHeaderButtonTheme } from "../theme/Button";
import { cardTheme } from "../theme/Card";

// Move to Forecast and rename
interface CardHeaderProps {
    markCompleted?: boolean
};

// Move to Forecast
export const CardForecastHeader = ({markCompleted}: CardHeaderProps) => {
    let uncompletedIcon = 'checkbox-blank-circle-outline';
    let completedIcon = 'checkbox-marked-circle';
    let icon = markCompleted ? completedIcon : uncompletedIcon;

    return (
        <>
            <View style={ cardTheme.header.completedContainer }>
                <Icon name={ icon } size={30} />
            </View>
            <View style={ cardTheme.header.headerTextForecastContainer }>
                <Text style={ cardTheme.header.headerText }>Journal Title</Text>
            </View>
            <Pressable style={ cardHeaderButtonTheme.container }>
                <Text style={ cardHeaderButtonTheme.text }>Go!</Text>
            </Pressable>
        </>
    );
};

// Make a props and add description to it.
const CardBasicHeader = () => {    
    return (  
        <View style={ cardTheme.header.headerTextContainer }>
            <Text style={ cardTheme.header.headerText }>Journal Title</Text>
        </View>
    );
};

interface CardProps {
    header: ReactNode,
    children: ReactNode
};

export const Card = ({header, children}: CardProps) => {
    return (
        <View style={cardTheme}>
            <View style={cardTheme.header}>
                {header}
            </View>
            {children}
        </View>
    );
};
// Add description to the props and pass it to CardBasicHeader
interface CardBasicProps {
    children: ReactNode
};

export const CardBasic = ({children}: CardBasicProps) => {
    return (
        <Card header={CardBasicHeader()}>
            {children}
        </Card>
    );
};
