import { Pressable, Text, View } from "react-native"
import { forecastStyle } from "../forecast/styles"
import Icon from "react-native-vector-icons/MaterialCommunityIcons"
import { cardHeaderButtonTheme } from "../theme/Button"

interface CardHeaderProps {
    markCompleted?: boolean
};

const CardForecastHeader = ({markCompleted}: CardHeaderProps) => {
    let uncompletedIcon = 'checkbox-blank-circle-outline';
    let completedIcon = 'checkbox-marked-circle';
    let icon = markCompleted ? completedIcon : uncompletedIcon;

    return (
        <View style={ forecastStyle.component.card.header }>
            <View style={ forecastStyle.component.card.header.completedContainer }>
                <Icon name={ icon } size={30} />
            </View>
            <View style={ forecastStyle.component.card.header.headerTextForecastContainer }>
                <Text style={ forecastStyle.component.card.header.headerText }>Journal Title</Text>
            </View>
            <Pressable style={ cardHeaderButtonTheme.container }>
                <Text style={ cardHeaderButtonTheme.text }>Go!</Text>
            </Pressable>
        </View>
    );
};

const CardHeader = ({markCompleted}: CardHeaderProps) => {
    let uncompletedIcon = 'checkbox-blank-circle-outline';
    let completedIcon = 'checkbox-marked-circle';
    let icon = markCompleted ? completedIcon : uncompletedIcon;

    return (
        <View style={ forecastStyle.component.card.header }>           
            <View style={ forecastStyle.component.card.header.headerTextContainer }>
                <Text style={ forecastStyle.component.card.header.headerText }>Journal Title</Text>
            </View>
        </View>
    );
};


export const Card = () => {
    return (
        <View style={forecastStyle.component.card}>
            <CardForecastHeader />
        </View>
    );
};